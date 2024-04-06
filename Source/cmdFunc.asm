;Note! Each function must setup the DTA to use for itself.
;There is no requirement to preserve the DTA across a call.
;Only the Int 2Eh entry point will preserve the callers DTA.

;Common Error Messages, jumped to to return from
badAccError:
    lea rdx, accDenMsg
    jmp short badCmn
noSelfCopyError:
    lea rdx, noSelfCopy
    jmp short badCmn
badParamError:
    lea rdx, badParm
    jmp short badCmn
badDriveError:
    lea rdx, badDrv
    jmp short badCmn
badArgError:
    lea rdx, badArgs
    jmp short badCmn
badFileError:
    lea rdx, badSpec
    jmp short badCmn
badDupFnf:
;Hybrid error message
    lea rdx, dupName
    jmp short badCmn
badFnf:
    lea rdx, fnfMsg
    jmp short badCmn
badDirError:
    lea rdx, badDir
badCmn:
    mov byte [returnCode], -1    ;Return code defaults to -1 if error (for now!)
    mov eax, 0900h
    int 21h
    stc ;Return with CY => Error occured
    return  ;This will be made nuanced later, to agree with DOS behaviour
badCmdError:
    lea rdx, badCmd
    jmp short badCmn
dir:
;Don't allow for searching unmounted network drives... is this a limitation?
    mov byte [dirFlags], 0    ;Clear DIR flags
    mov byte [dirLineCtr], 0
    mov byte [dirFileCtr], 0
    mov byte [dirSrchDir], 0
    mov word [searchSpec], 0
    lea rdi, dirSrchFCB ;Start also by initialising the search pattern
    mov byte [rdi + fcb.driveNum], 0    ;Default drive
    mov rax, "????????"
    mov qword [rdi + fcb.filename], rax
    mov dword [rdi + fcb.fileext], "???"
    ;Start by scanning for the switches
    lea rsi, qword [r8 + cmdLine]  ;Goto command line
    mov rdi, rsi    ;Use rsi as start of buffer counter
    call skipDelimiters ;Skip leading delimiters
    add rsi, 3  ;Go past the DIR
.scanNew:
    call skipDelimiters ;Set rsi pointing to a non delimiting char
    lodsb   ;Get this char
    cmp al, CR
    je .scanDone
    cmp al, byte [switchChar]   ;Was this the switch char?
    jne .notSwitch
    lodsb   ;Get the next char
    call ucChar
    cmp al, "W"
    jne .notWide
    or byte [dirFlags], dirWideType ;Set the correct bit
    jmp short .scanNew
.notWide:
    cmp al, "P"
    jne badParamError   ;If a switch other than /P or /W, fail
    or byte [dirFlags], dirPageType ;Set the correct bit
    jmp short .scanNew
.notSwitch:
    test byte [dirFlags], dirFileType
    jnz badParamError   ;If more than one path specified, error out
    ;Not a switch, so must be a pathspec, copy over to searchSpec
    dec rsi ;Point back to the char which was not a switchchar
    call copyArgumentToSearchSpec   
    dec rsi ;Point back to the terminator char
    or byte [dirFlags], dirFileType ;Now set path given bit
    jmp short .scanNew
.scanDone:
    test byte [dirFlags], dirFileType    ;If no path, use CWD for curdrv
    jz .currentDrv
    ;Here we check if we have a drvSpec and path or just drvSpec
    lea rsi, searchSpec
    cmp byte [rsi + 1], ":"  ;Is this a colon (drvspec check)
    jne .currentDrv
    ;Here the drive is specified, so lets parse filename to verify if drv ok
    mov byte [r8 + fcb1 + fcb.driveNum], 0  ;Clear this byte by default
    lea rdi, qword [r8 + fcb1]
    mov eax, 2901h   ;Parse filename
    int 21h
    cmp al, -1
    je badDriveError    ;If the drive is bad, bad parameter
    ;Else the drive in the fcb is valid
    movzx eax, byte [r8 + fcb1 + fcb.driveNum]
    test al, al
    jz .currentDrv
    dec al  ;Convert to 0 based drive number
    mov byte [dirDrv], al
    jmp short .dirPrintVol
.currentDrv:
    call getCurrentDrive    ;Get current drive number (0 based) in al
    mov byte [dirDrv], al   ;Store the 0 based drive number in al
.dirPrintVol:
    lea rsi, searchSpec
;Now construct the path on dirSrchDir.
    lea rdi, dirSrchDir
    movzx eax, byte [dirDrv] ;Get the 0 based drive number
    mov dl, al  ;Save the 0 based drive number in dl
    add al, "A" ;Turn into a letter
    mov ah, ":"
    stosw   ;Store X: into the search path
    mov eax, 121Ah  ;This will either move rsi forwards two or not... either is ok
    int 2fh
    cmp al, -1  ;Shouldn't happen at this point
    je badDriveError
    ;Now rsi is at the right point, if first char is a pathsep, we dont get cwd
    mov al, byte [pathSep]
    cmp byte [rsi], al  ;If this char is a pathsep, its an absolute path
    je .copyPathLoop    ;Just immediately start copying over chars
    stosb   ;Store the leading slash here and increment rdi by 1
    inc dl  ;Increment by 1
    xchg rsi, rdi   ;Store the cwd in rdi, save rsi
    mov eax, 4700h  ;Print current working directory
    int 21h
    xchg rsi, rdi   ;Get back the ptrs correctly.
    ;rsi points to the first char in the provided path.
    ;rdi points to the first char of the cwd.
    xor eax, eax
.goToEndOfCwd:
;Move to the end of the string, could use repne?
    cmp byte [rdi], al
    je .prepCopy    ;Exit when rdi points to the null
    inc rdi
    jmp short .goToEndOfCwd
.prepCopy:
    mov al, byte [pathSep]
    cmp byte [rsi], al
    je .copyPathLoop
    mov ah, ":"
    xchg al, ah
    cmp word [rdi - 2], ax
    je .copyPathLoop
    mov al, ah
    cmp byte [rdi - 1], al
    je .copyPathLoop
    stosb   ;Else, store a pathsep and inc rdi!
.copyPathLoop:
    lodsb
    cmp al, byte [pathSep]  ;Is it a pathsep?
    jne .checkDot  ;If not, store it
.lpPathseps:
    cmp byte [rsi], al  ;Is [rsi] a pathsep?
    jne .checkDot  ;If it isn't store al, check dots
    inc rsi
    jmp short .lpPathseps
.checkDot:
    cmp al, "."
    je .dots
.store:
    stosb
    test al, al ;Was the char we just copied a null?
    jz .exitPathCopy
    jmp short .copyPathLoop
.dots:
    ;Here handle dot and dot dot
    ;If the previous char is a pathsep, and the char after is a pathsep or null,
    ; this is a current dir. Keep rdi where it is. If next char is psep, adv rsi
    ; by 1. If it is null, go to top of loop.
    ;Check char before in rdi. If this is a pathsep, we get started.
    mov ah, byte [pathSep]
    cmp byte [rdi - 1], ah  
    jne .store  ;If this is not a pathsep, pass it up for storage. 
    mov ah, byte [rsi]  ;Now look ahead a char!
    cmp ah, "." ;Is this another dot?
    je .twoDot
    ;This is only a "current dir" when next char is pathsep or null
    cmp ah, byte [pathSep]
    je .skipThisPathsep
    test ah, ah
    jz .copyPathLoop
    jmp short .store ;Else, we store the char as normal!
.skipThisPathsep:
;If a dot is flanked by two pathseps, skip handling it and the pathsep!
    inc rsi ;Point to the pathsep
    jmp short .copyPathLoop ;and rejoin the store
.twoDot:
    ;Woohoo, this works!
    inc rsi ;Move past the second dot
    mov ah, byte [rsi]  ;Now pick up char after "." Valid only pathsep or null
    cmp ah, byte [pathSep]
    je .tdOk
    test ah, ah
    jne badParamError
.tdOk:
    mov al, byte byte [pathSep]
    cmp byte [rdi - 2], ":" ;rdi must be immediately past a pathsep here
    je badParamError
    sub rdi, 2
    xor ecx, ecx
    dec ecx
    std
    repne scasb ;Scan backwards for the pathsep
    cld
    add rdi, 2  ;Get on the right side of the pathsep
    test ah, ah ;If null, now pick it up and store it and exit properly
    jz .copyPathLoop
    inc rsi ;Else, go past the pathsep, pick up the next char as usual
    jmp .copyPathLoop ;And loop again
.exitPathCopy:
;Now we have the full, adjusted path copied over to dirSrchDir!
    sub rdi, 2  ;Go back two chars
    mov al, byte [pathSep]
    mov ah, ":"
    xchg al, ah
    cmp word [rdi - 1], ax    ;Is this a root dir?
    je .skipOverwrite
    mov al, ah
    cmp byte [rdi], al  ;Is this a trailing pathsep?
    jne .skipOverwrite
    mov byte [rdi], 0   ;Overwrite with a null
.skipOverwrite:
    lea rsi, dirSrchDir ;Now check if we have any WC's
    mov rdi, rsi
    mov eax, 1211h  ;Normalise path without affecting the registers
    int 2fh
    call scanForWildcards
    jz .wcSearchPattern     ;Wildcard found! We have a search pattern!
    mov rdx, rsi
    cmp byte [rsi + 3], 0   ;Is this a null path?
    je .wcSearchPattern     ;Move the ptr to end of path and add search patt.
.notNull:
    call setDTA ;Set the DTA
    mov ecx, dirDirectory   ;Dir, normal and read only files!
    mov eax, 4E00h ;Find first
    int 21h
    jc .wcSearchPattern ;If this errors, file not found, we have a search pattern
;Now we have searched for the file, is a directory?
    test byte [cmdFFBlock + ffBlock.attribFnd], dirDirectory
    jz .wcSearchPattern ;The end of the path is a search pattern
    ;Here we are searching IN a directory. Default search pattern!
    xor eax, eax
    xor ecx, ecx
    dec ecx
    repne scasb
    dec rdi ;Point rdi to the terminating null
    jmp short .wcDefaultSearch
.wcSearchPattern:
;Move the final portion of the path into the dirSrchFCB, overwriting the ?'s
;and place a null terminator on the pathsep before the final portion in dirSrchDir
    xor eax, eax
    xor ecx, ecx
    dec ecx
    repne scasb ;Scan forwards
    ;Here rdi points past the null at the end
    mov al, byte [pathSep]
    xor ecx, ecx
    dec ecx
    std ;Search backwards
    repne scasb
    cld ;Search forwards again
    inc rdi ;Go back the to the pathsep
    cmp byte [rdi - 1], ":" ;Is this the slash of the root dir?
    jne .wcsp1
    inc rdi ;Go one char forwards
.wcsp1:
    ;rdi points either on the pathsep or one char after the pathsep if in root dir
    push rdi
    cmp byte [rdi], al  ;If we are on pathsep, go forwards tmporarily
    jne .wcsp2
    inc rdi
.wcsp2:
    mov rsi, rdi    ;Now we source our chars from here
    lea rdi, dirSrchFCB
    mov eax, 290Dh  ;Modify only that which has been specified
    int 21h
    pop rdi
    xor eax, eax
    mov byte [rdi], al  ;Store the null terminator over the pathsep
.wcDefaultSearch:
    movzx eax, byte [dirDrv] 
    call volume.dirEP
    lea rdx, dirMain    ;Print message intro
    mov ah, 09h
    int 21h
    mov byte [rdi], "$"   ;Replace the null with a string terminator
    lea rdx, dirSrchDir   ;Print the directory we will work on
    mov ah, 09h
    int 21h
    mov byte [rdi], 0   ;Replace the null with a string terminator
    lea rdx, crlf2
    mov ah, 09h
    int 21h
;Now we copy the search pattern to look for into the path back from the fcb field.
;rdi points to the terminating null
    mov al, byte [pathSep]
    cmp byte [rdi - 1], al
    je .root
    inc rdi
.root:
    dec rdi ;Point back to the pathsep
    stosb   ;Store the pathsep here and advance rdi
    lea rsi, qword [dirSrchFCB + 1] ;Go to the name field of the FCB
    call FCBToAsciiz    ;Terminates for free
    call .searchForFile
    return

.searchForFile:
    call setDTA
    mov ecx, dirDirectory   ;Search for normal, ro and dir
    lea rdx, dirSrchDir
    mov eax, 4E00h ;Find first
    int 21h
    jc .dirNoMoreFiles
.findNext:
    call .dirPrintFileData  ;Print the file information
    mov ah, 4Fh
    int 21h
    jnc .findNext 
.dirNoMoreFiles:
    test byte [dirFlags], dirWideType
    jz .dirNoEndNewLine
    lea rdx, crlf   ;Only need this for /W
    mov ah, 09h
    int 21h
.dirNoEndNewLine:
    ;Now we print the number of files and the number of bytes on the disk
    lea rdx, fourSpc
    mov ah, 09h
    int 21h
    mov ah, 09h ;Print four Spaces twice
    int 21h
    movzx eax, byte [dirFileCtr]   ;Get number of files
    call printDecimalWord
    lea rdx, dirOk
    mov ah, 09h
    int 21h
    lea rdx, threeSpc
    mov ah, 09h
    int 21h
    mov eax, 3600h ;Get disk info
    mov dl, byte [dirDrv]
    inc dl  ;Function 36h wants the 1 based number
    int 21h ;Get disk free space info
    movzx eax, ax   ;Sectors per Cluster 
    movzx ecx, cx   ;Bytes per Sector
    or ebx, ebx ;Clear the upper bits of rbx
    mul ecx ;Get bytes per cluster
    mul rbx ;Multiply to the number of free clusters on the disk
    ;rax now has the number of free bytes on the disk
    call printDecimalWord
    lea rdx, bytesOk
    mov ah, 09h
    int 21h
    return

.dirPrintFileData:
;Use fcbCmdSpec to build the file name with space
;Start by print the name (same for both cases)
;We first check if the file has attributes hidden/system and hide them if so
    test byte [cmdFFBlock + ffBlock.attribFnd], dirIncFiles
    retnz   ;Simply return if either bit is set
    lea rsi, qword [cmdFFBlock + ffBlock.asciizName]
    lea rdi, fcbCmdSpec
    call asciiFilenameToFCB
    lea rdx, fcbCmdSpec
    mov ecx, 8  ;Print 8 chars
    mov ebx, 1  ;STDOUT
    mov ah, 40h ;Write handle
    int 21h
    push rdx
    mov dl, " "
    mov ah, 02h ;Print char
    int 21h
    pop rdx
    add rdx, 8  ;Go to ext field
    mov ecx, 3  ;Print three chars
    mov ebx, 1  ;STDOUT
    mov ah, 40h ;Write handle
    int 21h
    test byte [dirFlags], dirWideType
    jnz .widePrint
;Normal print (Name space ext <> File size <> Acc Date <> Acc Time)
    ;Now check if a DIR
    test byte [cmdFFBlock + ffBlock.attribFnd], dirDirectory
    jz .dirPrintNotDir
    lea rdx, dirLbl
    mov ah, 09h
    int 21h
    lea rdx, threeSpc
    mov ah, 09h
    int 21h
    jmp short .dirPrintFileDT
.dirPrintNotDir:
;Here we print the file size
    mov dl, " "
    mov ah, 02h
    int 21h
    mov eax, dword [cmdFFBlock + ffBlock.fileSize]
    call getDecimalWord
    mov rbx, rcx
    push rcx
    bswap rbx
    mov ecx, 8
.dirPrintFileSizePrep:
    test bl, bl ;Any leading null's get replaced with a space
    jne .dirPrintFileSize
    mov ah, 02h
    mov dl, " "
    int 21h
    shr rbx, 8  ;Get next byte
    dec ecx
    cmp ecx, 1
    jne .dirPrintFileSizePrep   ;Always print 1 byte for size
.dirPrintFileSize:
    pop rbx
.dirPrintFileSizeLoop:
    mov dl, bl
    mov ah, 02h
    int 21h
    shr rbx, 8  ;Get next byte
    dec ecx
    jnz .dirPrintFileSizeLoop
    lea rdx, twoSpc
    mov ah, 09h
    int 21h
.dirPrintFileDT:
    mov dl, " "
    mov ah, 02h
    int 21h
    movzx eax, word [cmdFFBlock + ffBlock.fileDate]
    xor ebx, ebx    ;Ensure we print 2 digit year
    call printDate
    lea rdx, twoSpc
    mov ah, 09h
    int 21h
    movzx eax, word [cmdFFBlock + ffBlock.fileTime]
    call printTime
    lea rdx, crlf
    mov ah, 09h
    int 21h
    jmp short .dirPrintNameExit
.widePrint:
;If /W, print name space ext space space space space
    lea rdx, fourSpc
    mov ah, 09h ;Print string
    int 21h
.dirPrintNameExit:
    inc byte [dirFileCtr]   ;Increment file counter
    inc byte [dirLineCtr]
    cmp byte [dirLineCtr], 23
    retne
    lea rdx, pauseMes
    mov ah, 09h
    int 21h
    mov ah, 01h ;Wait for a char from STDIN
    int 21h
    mov byte [dirLineCtr], 0
    lea rdx, crlf   ;Force new line
    mov ah, 09h
    int 21h
    return

chdir:
    test byte [arg1Flg], -1
    jnz .changeDir
    ;Print CWD
.printCWD:
    call putCWDInPrompt ;Exactly the same procedure
    call printCRLF
    return
.printDiskCWD:
;Print CWD for a specified drive
    mov dl, byte [r8 + fcb1 + fcb.driveNum] ;Get 1 based drive number in dl
    mov al, dl
    add al, "@" ;Get the UC letter
    mov ah, ":"
    lea rdi, searchSpec
    stosw   ;Store X:, rdi+=2
    mov al, byte [pathSep]
    stosb   ;Store pathSep, inc rdi
    mov ah, 47h ;Get Current Working Directory
    mov rsi, rdi    ;rsi points to buffer to write to
    int 21h
    call strlen
    add ecx, 2 ;Add two for the X:
    mov ah, 40h ;Write to handle
    mov ebx, 1  ;STDOUT
    lea rdx, searchSpec
    int 21h
    call printCRLF
    return
.changeDir:
    mov al, byte [arg1FCBret]
    cmp al, -1 
    je badDriveError  ;IF the drive is good, but FCB name blank, either X: or \ 
    call buildCommandPath   ;Else build a fully qualified pathname
    lea rdx, searchSpec
    mov ah, 3Bh ;CHDIR
    int 21h
    jc badDirError
    return

mkdir:
    test byte [arg1Flg], -1
    jz badArgError
    test byte [arg2Flg], -1
    jnz badArgError
    ;We have exactly one argument
    mov al, byte [arg1FCBret]
    cmp al, -1 
    je badDriveError  ;If a drive was specified and was bad, jump
    call buildCommandPath
    lea rdx, searchSpec
    mov eax, 3900h  ;MKDIR
    int 21h
    retnc
.badMake:   ;Else, bad make
    lea rdx, badMD
    mov eax, 0900h
    int 21h
    return

rmdir:
    test byte [arg1Flg], -1
    jz badArgError
    test byte [arg2Flg], -1
    jnz badArgError
    ;We have exactly one argument
    mov al, byte [arg1FCBret]
    cmp al, -1 
    je badDriveError  ;If a drive was specified and was bad, jump
    call buildCommandPath
    lea rdx, searchSpec
    mov eax, 3A00h  ;RMDIR
    int 21h
    retnc   ;Return if not carry
.badRemove:   ;Else, bad make
    lea rdx, badRD
    mov eax, 0900h
    int 21h
    return

copy:
    test byte [arg1Flg], -1
    jz badArgError
    test byte [arg2Flg], -1
    jz badArgError
    movzx eax, byte [arg1Off]
    mov r8, [pspPtr]
    lea rsi, qword [r8 + cmdLine]
    mov rbx, rsi    ;Save the ptr to the start of the string in rbx
    add rsi, rax    ;Go to the start of the command
    lea rdi, srcSpec
    call cpDelimPathToBufz    
    movzx eax, byte [arg2Off]
    mov rsi, rbx    ;Get back the start of the ptr
    add rsi, rax    ;Go to the start of the command
    lea rdi, destSpec
    call cpDelimPathToBufz   
;Before we open, we check if the two filenames are equal
; If so, crap out.
    lea rsi, srcSpec
    lea rdi, destSpec
    mov eax, 121Eh
    int 2Fh
    jz .sameFilename
    ;Open source with read permission
    ;Open destination with write permission
    lea rdx, srcSpec
    mov eax, 3D00h  ;Read open
    int 21h
    jc badParamError
    mov word [sourceHdl], ax

    movzx ebx, ax   ;For bx
    mov eax, 4400h  ;Get device info in dx
    int 21h
    mov word [srcHdlInfo], dx   ;Store information here

    lea rdx, destSpec
    mov eax, 3C00h  ;Create the file
    xor ecx, ecx    ;No file attributes
    int 21h
    jc .badExit
    mov word [destHdl], ax
    xor esi, esi
    lea rdx, copyBuffer
.copyLoop:
    mov ecx, 128
    movzx ebx, word [sourceHdl]
    mov ah, 3Fh ;Read
    int 21h
    jc .badExit
    test eax, eax
    jz .okExit
    add esi, eax
    mov ecx, eax
    movzx ebx, word [destHdl]
    mov ah, 40h ;Write
    int 21h
    jc .badExit
    cmp eax, 128    ;Did we read 128 chars?
    je .copyLoop
    ;If not char dev, exit
    test word [srcHdlInfo], 80h ;Char dev bit set?
    jz .okExit
    ;Is handle in cooked or binary mode?
    test word [srcHdlInfo], 20h
    jnz .okExit
    ;Here the char dev must be in cooked mode. Check if the last char was ^Z
    or eax, eax ;Clear upper bits in eax
    lea rdi, qword [rdx + rax - 1]  ;Point to the last char in the buffer
    cmp byte [rdi], EOF ;Was this EOF?
    jne .copyLoop   ;Jump if not
.okExit:
    call .leaveCopyClose
    lea rdx, crlf
    mov ah, 09h
    int 21h
    lea rdx, fourSpc
    mov ah, 09h
    int 21h
    mov ah, 02h
    mov dl, "1" ;1 File(s) copied
    int 21h
    lea rdx, copyOk
    mov ah, 09h
    int 21h
    return
.sameFilename:
    call .leaveCopyClose ;Close the handles
    jmp noSelfCopyError
.leaveCopyClose:
    mov bx, word [sourceHdl]
    mov eax, 3E00h
    int 21h
    mov bx, word [destHdl]
    mov eax, 3E00h
    int 21h
    return
.badExit:
;Prototypically use badParamError for error reporting... sucks I know
    mov bx, word [sourceHdl]
    cmp bx, -1
    je .skipSource
    mov eax, 3E00h  ;Close this handle
    int 21h
.skipSource:
    mov bx, word [destHdl]
    cmp bx, -1
    je badParamError
    mov eax, 3E00h
    int 21h
    jmp badParamError

erase:
    test byte [arg1Flg], -1
    jz badArgError
    call setDTA     ;Start by resetting the DTA
    call buildCommandPath   ;Get the relative path to the file
.dirLp:
    lea rdi, searchSpec
    call findLastPathComponant
    mov rsi, rdi    ;Save this as the source
    lea rdi, qword [r8 + fcb1]  ;Use FCB1 for searchpath res
    mov al, "?"
    ;Store question marks in the name field
    push rdi
    inc rdi ;Goto the first char
    mov ecx, 11
    rep stosb
    pop rdi
    mov rdx, rsi    ;Save the ptr
    mov eax, 290Dh  ;Parse the search mask into the FCB block
    int 21h
    cmp al, 1
    jne .noWildcard ;If no wildcards, just delete directly, unless a directory
    ;Else, we now copy back the search pattern over the last componant!
    ;No dirs to handle in here
    lea rsi, qword [rdi + fcb.filename]    ;Now move the ptr to the filename in rsi
    mov rdi, rdx    ;Move the ptr to the start of the last componant to rdi
    call FCBToAsciiz    ;Null terminates for free
    ;Count the number of ?'s, if 11, we print the message
    lea rsi, qword [r8 + fcb1 + fcb.filename]
    xor ecx, ecx
.wcScan:
    lodsb
    cmp al, "?"
    jne .endCount
    inc ecx
    cmp ecx, 11
    jne .wcScan
.ynmsg:
    lea rdx, ynMes
    call printString
    mov ah, 01h ;STDIN without Console Echo
    int 21h ;Get char in al
    call ucChar ;Uppercase the char
    cmp al, "Y" ;If they want it... they'll get it!
    je .endCount1
    cmp al, "N"
    rete    ;Simply return to command line if they don't want it!
    jmp short .ynmsg    ;Else, tell me what you want!!!
.endCount1:
    call printCRLF
.endCount:
    ;Now we copy our search template pathstring to delPath
    lea rdi, delPath
    lea rsi, searchSpec ;Source the chars from here
    call strcpy2         ;Copy the string over to delPath
.findFile:
    ;Now we find first/find next our way through the files
    mov rdx, rsi    ;rdx must point at searchSpec for findfirst
    xor ecx, ecx    ;Search for normal and ro files only
    mov eax, 4E00h  ;Find first
    int 21h
    jc badFnf   ;Here we just print file not found error and return!
    ;Now the file was found, we copy the name over, delete and keep going
    call findLastPathComponant  ;Now find the last componant in delPath
    lea rsi, qword [cmdFFBlock + ffBlock.asciizName]
.delNextFile:
;rsi and rdi dont move here
    call strcpy2     ;Now copy over ASCIIZname to last path componant of delpath
    lea rdx, delPath
    mov eax, 4100h  ;Delete File 
    int 21h         ;If this fails to delete it, fail silently
    lea rdx, searchSpec    ;Now point rdx to the search spec
    mov eax, 4F00h  ;Else, find next file
    int 21h
    jnc .delNextFile    
    clc ;Clear carry to indicate success
    return
.noWildcard:
    ;Here we just check that the file was not a directory. If it was, we add
    ; a \*.*<NUL> over the null terminator
    lea rdx, searchSpec
    mov ecx, dirDirectory    ;Search for normal, RO and dir
    mov eax, 4E00h  ;Find first
    int 21h
    jc badFnf   ;Here we just print file not found error and return!
    test byte [cmdFFBlock + ffBlock.attribFnd], dirDirectory
    jz .delMain ;If not a dir, must be a file, delete it directly!
    ;Else, we are dealing with a dir
    mov rdi, rdx
    xor ecx, ecx
    dec ecx
    repne scasb ;Go to the end of the line!
    dec rdi
    movzx eax, byte [pathSep]
    stosb
    mov eax, "*.*"  ;Null terminated for us!
    stosd
    jmp .dirLp    ;Now restart the process with extended path!
.delMain:   ;Call with rdx -> buffer!
    mov eax, 4100h  ;Delete File 
    int 21h
    retnc   ;Return if CF=NC
    cmp al, errAccDen
    je badAccError ;If the file is RO, fail!
    jmp badFileError
    return
date:
    lea rdx, curDate
    mov ah, 09h
    int 21h
    mov ah, 2Ah ;DOS get date
    int 21h
	;AL = day of the week (0=Sunday)
	;CX = year (1980-2099)
	;DH = month (1-12)
	;DL = day (1-31)
    mov word [td1], cx
    mov byte [td3], dl
    mov byte [td4], dh
    movzx eax, al
    mov ebx, eax
    shl ebx, 1   ;Multiply by 2
    add eax, ebx ;Make it 3 times 
    lea rdx, dayName
    lea rdx, qword [rdx + rax]  ;Go to the right day name
    mov ecx, 3  ;Print three chars
    mov ebx, 1  ;STDOUT
    mov ah, 40h ;Write to handle
    int 21h
    mov dl, " "
    mov ah, 02h
    int 21h
;       eax[0:4] = Day of the month, a value in [0,...,31]
;       eax[5:8] = Month of the year, a value in [0,...,12]
;       eax[9:15] = Number of years since 1980, a value in [0,...,127]
    movzx eax, word [td1]   ;Get this word
    shl eax, 9 ;Move it high to pack it properly
    movzx ebx, byte [td4]
    shl ebx, 5  ;Shift the date to the right position
    or eax, ebx ;Add this date to eax
    movzx ebx, byte [td3]
    or eax, ebx
    mov ebx, 1  ;Four digit year pls
    call printDate

    lea rdx, newDate
    mov ah, 09h
    int 21h
    lea rdx, ukDate
    lea rax, usDate
    lea rbx, jpDate
    cmp byte [ctryData + countryStruc.dtfmt], 1
    cmova rdx, rbx
    cmovb rdx, rax
    mov ah, 09h
    int 21h

    lea rdx, inBuffer
    mov byte [rdx], 126 ;Enter a string of up to 126 chars in length
    mov ah, 0Ah
    int 21h
    push rdx
    lea rdx, crlf
    mov ah, 09h
    int 21h
    pop rdx
    cmp byte [rdx + 1], 0   ;If the user typed nothing...
    rete    ;Exit!
    return

time:
    lea rdx, curTime
    mov ah, 09h
    int 21h
    mov ah, 2Ch ;DOS get time
    int 21h
    ;CH = hour (0-23)
	;CL = minutes (0-59)
	;DH = seconds (0-59)
	;DL = hundredths (0-99)
    mov byte [td1], cl
    mov byte [td2], ch
    mov byte [td3], dl
    mov byte [td4], dh
    movzx eax, ch
    call printTime.printHours

    mov dl, byte [ctryData + countryStruc.timeSep]
    mov ah, 02h
    int 21h

    movzx eax, byte [td1]   ;Minutes
    call printTime.printMinutesAlt

    mov dl, byte [ctryData + countryStruc.timeSep]
    mov ah, 02h
    int 21h

    movzx eax, byte [td4]   ;Seconds
    call printTime.printMinutesAlt

    mov dl, "."
    mov ah, 02h
    int 21h

    movzx eax, byte [td3]   ;Hundreths
    call printTime.printMinutesAlt

    lea rdx, newTime
    mov ah, 09h
    int 21h

    lea rdx, inBuffer
    mov byte [rdx], 126 ;Enter a string of up to 126 chars in length
    mov ah, 0Ah
    int 21h
    push rdx
    lea rdx, crlf
    mov ah, 09h
    int 21h
    pop rdx
    cmp byte [rdx + 1], 0   ;If the user typed nothing...
    rete    ;Exit!
    return
ctty:
    test byte [arg1Flg], -1
    jz badArgError
    test byte [arg2Flg], -1
    jnz badArgError
    lea rsi, qword [r8 + cmdLine]
    movzx eax, byte [arg1Off]
    add rsi, rax  ;Goto the first char of the argument
    cmp byte [rsi + 1], ":" ;If a drive is specified, check if valid
    jne .noDrive
    movzx eax, byte [arg1FCBret]
    cmp al, -1
    je badDriveError
.noDrive:
    ;Now we open the provided file
    call copyArgumentToSearchSpec
    lea rdx, searchSpec
    mov eax, 3D02h  ;Open in read/write mode
    int 21h
    jc badFileError
    movzx ebx, ax   ;Save the handle in ebx
    mov eax, 4400h  ;Get device word
    int 21h
    test dl, 80h    ;Test if this device is a char device
    jz .badCharDev  ;If this bit is 0 => Disk file
    ;Now we set this handle to be STDIO
    or dl, 3    ;Set STDIO bits
    xor dh, dh
    mov eax, 4401h  ;Now we set the device word
    int 21h
    ;Now we DUP2 for STDIN/OUT/ERR
    xor ecx, ecx    ;STDIN
    mov ah, 46h
    int 21h
    inc ecx         ;STDOUT
    mov ah, 46h
    int 21h
    inc ecx         ;STDERR
    mov ah, 46h
    int 21h
    mov ah, 3Eh ;Now we close the original handle
    int 21h
    return
.badCharDev:
    lea rdx, badDev
    mov ah, 09h
    int 21h
    mov ah, 3Eh ;Close opened handle
    int 21h
    return

cls:  
    mov eax, 4400h  ;Get device info
    mov ebx, 1      ;for handle 1
    int 21h         ;in dx
    test edx, devCharDev
    jz .doAnsi  ;Make files register an ansi cls sequence
    test edx, charDevFastOut
    jz .doAnsi
    ;Test if Int 29h uses Int 30h
    ;Tests if within the first 1024 bytes we have the sequence Int 30h (30CD)
    ;Int 29h MUST be terminated with a IRETQ, within 1024 bytes
    mov eax, 3529h  ;Get the vector for interrupt 29h
    int 21h
.biosCheck:
    cmp word [rbx], 30CDh
    je .biosConfirmed
    cmp word [rbx], 0CF48h   ;CFh = IRET, 48h=REX.W
    je .doAnsi
    inc rbx
    jmp short .biosCheck
.biosConfirmed:
    ;Supports a SCP/BIOS compatible routine, use BIOS   
    mov ah, 0Bh  ; Set overscan to black (when Graphics becomes supported)
    xor ebx, ebx
    int 30h
    mov ah, 0Fh ;Get screen mode
    int 30h
    movzx edx, ah   ;Get number of columns in dl
    dec dl
    mov dh, 25  ;Number of rows is standard
    xor eax, eax
    mov ecx, eax
    mov bh, 7   ;Screen attributes
    mov ah, 6   ;Scroll
    int 30h
    xor edx, edx    ;Set cursor coordinates to top left of screen
    mov bh, 0   ;Page 0
    mov ah, 2
    int 30h
    return
.doAnsi:
;4 chars in the ansi routine
;Will just put the ANSI escape sequence on the screen if it doesn't 
; understand ANSI codes
    lea rsi, ansiCls
    mov ecx, 4
    mov ah, 06h ;Raw char output
.ansiLp:
    lodsb   ;Get the char in 
    mov dl, al
    int 21h
    dec ecx
    jnz .ansiLp
    return

break:
    test byte [arg1Flg], -1
    jnz .argumentProvided
    ;Here we just get the status of break
    mov eax, 3300h  ;Get break status in dl
    int 21h
    mov bl, dl
    lea rdx, breakIs
    mov ah, 09h
    int 21h
    lea rdx, onMes
    lea rcx, offMes
    test bl, bl ;IF bl = 0, break is off
    cmovz rdx, rcx
    mov ah, 09h
    int 21h
    return
.argumentProvided:
    lea rsi, qword [r8 + fcb1 + fcb.filename]  ;Point to the first fcb name
    lodsd   ;Read the word
    mov ebx, eax
    and eax, 0DFDFh  ;Convert first two chars to uppercase
    shr ebx, 10h     ;Get high word low
    cmp bx, "  " ;Two spaces is a possible ON 
    je .maybeOn
    cmp ax, "OF"
    jne .badOnOff
    and bx, 0FFDFh ;Convert only the third char to UC. Fourth char MUST BE SPACE
    cmp bx, "F "
    jne .badOnOff
    ;Set off
    xor edx, edx    ;DL=0 => BREAK is off
    jmp short .setBreak
.maybeOn:
    cmp ax, "ON"
    jne .badOnOff
    ;Set on
    mov edx, 1
.setBreak:
    mov eax, 3301h  ;Set break
    int 21h
    return
.badOnOff:
    lea rdx, badOnOff
    mov ah, 09h
    int 21h
    return

verify:
    test byte [arg1Flg], -1
    jnz .argumentProvided
    ;Here we just get the status of break
    mov eax, 5400h  ;Get verify status in al
    int 21h
    mov bl, al
    lea rdx, verifyIs
    mov ah, 09h
    int 21h
    lea rdx, onMes
    lea rcx, offMes
    test bl, bl ;IF bl = 0, break is off
    cmovz rdx, rcx
    mov ah, 09h
    int 21h
    return
.argumentProvided:
    lea rsi, qword [r8 + fcb1 + fcb.filename]  ;Point to the first fcb name
    lodsd   ;Read the word
    mov ebx, eax
    and eax, 0DFDFh  ;Convert first two chars to uppercase
    shr ebx, 10h     ;Get high word low
    cmp bx, "  " ;Two spaces is a possible ON 
    je .maybeOn
    cmp ax, "OF"
    jne .badOnOff
    and bx, 0FFDFh ;Convert only the third char to UC. Fourth char MUST BE SPACE
    cmp bx, "F "
    jne .badOnOff
    ;Set off
    xor eax, eax    ;AL=0 => VERIFY is off
    jmp short .setVerify
.maybeOn:
    cmp ax, "ON"
    jne .badOnOff
    ;Set on
    xor eax, eax
    inc eax ;AL=1 => VERIFY is on
.setVerify:
    mov ah, 2Eh  ;Set Verify
    int 21h
    return
.badOnOff:
    lea rdx, badOnOff
    mov ah, 09h
    int 21h
    return

rename:
    test byte [arg1Flg], -1
    jz badArgError
    test byte [arg2Flg], -1
    jz badArgError    
    ;Initialise the variables!
    lea rsi, srcSpec
    mov qword [srcPtr], rsi
    lea rsi, destSpec
    mov qword [destPtr], rsi
    ;Check the second path is just a filename!
    movzx eax, byte [arg2Off]
    mov r8, qword [pspPtr]
    lea rsi, qword [r8 + cmdLine]
    add rsi, rax    ;Point to the path componant
    lea rdi, searchSpec
    push rdi
    call cpDelimPathToBufz  ;Copy the path to rdi
    pop rdi
    cmp byte [rdi + 1], ":"  ;Is this a drive sep?
    je badArgError  ;Cannot pass a path as the second argument
    mov rsi, rdi
.destScan:
    lodsb   ;Get the char
    cmp al, byte [pathSep]  ;Ensure no path seps
    je badArgError
    test al, al
    jnz .destScan
;Now store question marks over all of fcb1 and fcb2's name and drive specifier
    lea rdi, qword [r8 + fcb1]  ;Use FCB1 for searchpath res
    mov rax, "????????"
    mov ecx, 3  ;24 bytes
    rep stosq
    stosd   ;Plus 4 to get 28 = 16 + 1 + 11
    lea rsi, searchSpec
    lea rdi, qword [r8 + fcb2]  ;Place destination pattern here
    mov eax, 290Dh
    int 21h     
    push rax    ;Save the WC signature
    call buildCommandPath   ;Copy the source path+searchspec to searchSpec
    lea rdi, searchSpec
    call findLastPathComponant  ;Go to the end of the path in rdi
    mov rsi, rdi
    mov rbx, rdi    ;Save the componant in rbx
    lea rdi, qword [r8 + fcb1]  ;Now copy source pattern over!
    mov eax, 290Dh
    int 21h
    pop rcx ;Get back the WC signature from the destination path!
    mov ah, cl
    test ax, ax ;If neither path has wildcards, go straight to renaming
    jz .noWC
    lea rsi, searchSpec ;Are we are the head of the buffer?
    ;Now we check if we have a path to actually handle
    cmp rbx, rsi
    je .noPath
    mov byte [rbx - 1], 0   ;The previous char is a pathsep, overwrite it!
    lea rsi, searchSpec
    lea rdi, srcSpec
    push rsi
    call strcpy ;Copy the string, leave the ptrs past the null
    pop rsi
    dec rdi     ;Point rdi to the null
    mov al, byte [pathSep]
    stosb       ;Store the pathsep and advance the pointer
    mov qword [srcPtr], rdi ;Store this ptr here
    lea rdi, destSpec
    call strcpy ;Copy the string, leave the ptrs past the null
    dec rdi     ;Point rdi to the null
    stosb       ;Store the pathsep and adv the ptr
    mov qword [destPtr], rdi
    mov byte [rbx - 1], al  ;Now replace the null with a pathsep again!
.noPath:
    ;Now we have where to copy the files to, we can start our work!
    call setDTA
    lea rdx, searchSpec
    xor ecx, ecx    ;Rename works on normal and read only files only!
    mov eax, 4E00h  ;Find first!
    int 21h
    jc badDupFnf    ;If no file was found!
.wcLoop:
    ;Now build an FCB form of the name of the file we found in fcb1
    lea rsi, cmdFFBlock + ffBlock.asciizName
    lea rdi, qword [r8 + fcb1]
    mov eax, 2901h  ;Now we copy the name over, cleaning the FCB name
    int 21h 
    ;Now depending on source and dest chars, we build a filename in renName
    lea rsi, qword [r8 + fcb2 + fcb.filename]    
    lea rdi, renName    ;Start by copying the destination pattern
    push rdi
    movsq
    movsw
    movsb
    pop rdi
    lea rsi, qword [r8 + fcb1 + fcb.filename] ;Now start sourcing chars!
    mov ecx, 11 ;11 chars to copy!
.wcNameMake:
    lodsb   ;Get the char from the source string
    cmp byte [rdi], "?" ;Are we over a WC?
    jne .noStore    ;Dont store the char there
    mov byte [rdi], al  ;If we are over a wc, store the char there!
.noStore:
    inc rdi ;Goto next char position
    dec ecx
    jnz .wcNameMake
;renName now has the FCB name for the file we wish to make!
;Now make the two paths!
    lea rsi, cmdFFBlock + ffBlock.asciizName
    mov rdi, qword [srcPtr]
    call strcpy ;Doesn't matter that we preserve the pointers
    lea rsi, renName
    mov rdi, qword [destPtr]
    call FCBToAsciiz
    lea rdx, srcSpec
    lea rdi, destSpec
    mov eax, 5600h
    int 21h     ;Fail silently on wildcard rename
    mov eax, 4F00h  ;Find next
    int 21h
    jnc .wcLoop     ;And process it too!
    return          ;Else just return!
.noWC:
    call buildCommandPath   ;Copy the source arg into searchspec
    movzx eax, byte [arg2Off]
    lea rsi, qword [r8 + cmdLine]
    add rsi, rax    ;Go to the start of the command to source chars
    lea rdi, destSpec
    push rdi
    call cpDelimPathToBufz
    pop rdi
    lea rdx, searchSpec
    mov eax, 5600h
    int 21h
    retnc   ;Return if all oki!
    jmp badDupFnf   ;Always just return this


touch:
;Temporarily used to create files
    test byte [arg1Flg], -1
    jz badArgError
    call buildCommandPath
    lea rdx, searchSpec
    mov eax, 5B00h  ;Create unique file 
    xor ecx, ecx
    int 21h
    jc .touch1
.touchClose:
    movzx ebx, ax   ;Save the handle here
    mov eax, 120Dh  ;Get date/time words from the DOS
    int 2fh
    mov ecx, eax    ;Move the time here
    xchg edx, ecx   ;Get them in the right place
    mov eax, 5701h  ;Set the date/time for bx
    int 21h
    mov eax, 3e00h  ;Close file immediately
    int 21h
    return
.touch1:
    cmp al, errFilExist ;Does the file exist?
    jne .touchError ;If not, this is a proper error!
    mov eax, 3D00h  ;R/O open instead to update the access time!!
    int 21h
    jnc .touchClose ;If this worked, close the handle immediately
.touchError:
    cmp al, errPnf
    je badArgError
    cmp al, errFnf
    je badFnf
    jmp badAccError
    

join:
;Mock join command, to test join. Make an external command.
;Mounts the A drive on C:\JOINTEST,0
    test byte [.joinOn], -1
    jz .okJoin
.joindisable:
    mov eax, 5200h  
    int 21h
    ;Set the join flag on A and make CDS path C:\JOINTEST,0
    mov eax, 8001h  ;Enter crit 1
    int 2Ah
    lea rbp, qword [rbx + 61h]  ;Get ptr to join byte
    mov rbx, qword [rbx + 2Ah]  ;Get CDS ptr
    and word [rbx + cds.wFlags], ~cdsJoinDrive    ;Clear that we are join
    mov byte [rbx], "A"     ;Set back to A
    mov byte [rbx + 3], 0   ;Terminating Nul
    dec byte [rbp]          ;Decrement DOS counter
    mov eax, 8101h  ;Exit crit 1
    int 2Ah
    mov byte [.joinOn], 0
    lea rdx, .joinDisableMsg
    jmp short .joinExit
.okJoin:
    mov byte [.joinOn], -1
    mov eax, 5200h  
    int 21h
    lea rbp, qword [rbx + 61h]  ;Get ptr to join byte
    mov rbx, qword [rbx + 2Ah]  ;Get CDS ptr
    ;Set the join flag on A and make CDS path C:\JOINTEST,0
    mov eax, 8001h  ;Enter crit 1
    int 2Ah
    or word [rbx + cds.wFlags], cdsJoinDrive    ;Set that we are join
    mov rdi, rbx
    lea rsi, .joinPath
    mov ecx, .joinPathL
    rep movsb   ;Copy chars over
    inc byte [rbp]  ;Increment DOS counter
    mov eax, 8101h  ;Exit crit 1
    int 2Ah
    lea rdx, .joinEnableMsg
.joinExit:
    mov eax, 0900h
    int 21h
    return
.joinEnableMsg:  db CR,LF,"JOIN enabled",CR,LF,"$"
.joinDisableMsg: db CR,LF,"JOIN disabled",CR,LF,"$"
.joinOn: db 0    ;Var to indicate we are on
.joinPath:  db "C:\JOINTEST",0
.joinPathL  equ $ - .joinPath 

subst:
;Mock subst command, to test SUBST. Make an external command.
;Substitutes C:\SUBTEST,0 for "D:\"
    test byte [.substOn], -1
    jz .okSubst
.substdisable:
    mov eax, 5200h  
    int 21h
    ;Set the SUBST and valid flags on D: and make CDS path C:\SUBSTEST,0
    mov eax, 8001h  ;Enter crit 1
    int 2Ah
    mov rbx, qword [rbx + 2Ah]  ;Get CDS ptr
    add rbx, 3*cds_size ;Go to the fourth CDS
    and word [rbx + cds.wFlags], ~(cdsSubstDrive | cdsValidDrive)    ;Clear that we are subst (and valid)
    mov byte [rbx], "D"     ;Set back to D
    mov byte [rbx + 3], 0   ;Terminating Nul
    mov word [rbx + cds.wBackslashOffset], 2
    mov eax, 8101h  ;Exit crit 1
    int 2Ah
    mov byte [.substOn], 0
    lea rdx, .substDisableMsg
    jmp short .substExit
.okSubst:
    mov byte [.substOn], -1
    mov eax, 5200h  
    int 21h
    mov eax, 8001h  ;Enter crit 1
    int 2Ah
    mov rbx, qword [rbx + 2Ah]  ;Get CDS ptr
    add rbx, 2*cds_size ;Go to the third CDS to get the DPB ptr
    mov rax, qword [rbx + cds.qDPBPtr]
    add rbx, cds_size   ;Now go the fourth CDS
    mov qword [rbx + cds.qDPBPtr], rax
    mov dword [rbx + cds.dStartCluster], 59h ;Hardcoded, read from the FAT table
    ;Set the subst flag on D and make CDS path C:\SUBSTEST,0
    or word [rbx + cds.wFlags], cdsSubstDrive | cdsValidDrive   ;Set that we are join
    mov word [rbx + cds.wBackslashOffset], .substPathL - 1
    mov rdi, rbx
    lea rsi, .substPath
    mov ecx, .substPathL
    rep movsb   ;Copy chars over
    mov eax, 8101h  ;Exit crit 1
    int 2Ah
    lea rdx, .substEnableMsg
.substExit:
    mov eax, 0900h
    int 21h
    return
.substEnableMsg:  db CR,LF,"SUBST enabled",CR,LF,"$"
.substDisableMsg: db CR,LF,"SUBST disabled",CR,LF,"$"
.substOn: db 0    ;Var to indicate we are on
.substPath:  db "C:\SUBSTEST",0
.substPathL  equ $ - .substPath 
;TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP
truename:
    test byte [arg1Flg], -1
    jz badArgError
    call buildCommandPath   ;Get the first argument into the searchSpec
    lea rsi, searchSpec     ;Store the path here
    mov rdi, rsi    ;Normalise the pathspec here
    mov eax, 6000h  ;TRUENAME
    int 21h
    jnc .writePath
    cmp al, errFnf
    je badFileError
    jmp badParamError
.writePath:
    mov rdx, rdi    ;Print from the destination buffer
    mov ecx, -1
    xor al, al
    repne scasb     ;Get the new len
    not ecx         ;Invert bits and subtract by 1 (to drop trailing 0)
    dec ecx
    mov ebx, 01
    mov ah, 40h
    int 21h
    call printCRLF
    return

label:
;Displays/Changes/Deletes the file label
    lea rdx, .l1
    mov eax, 0900h
    int 21h
    return
.l1: db CR,LF,"Function unimplemented",CR,LF,"$"

mklbl:
    ;lea rdx, .l1
    ;mov cx, 08h ;Make a label
    ;mov eax, 3C00h
    ;int 21h

    ;mov ebx, eax
    ;mov eax, 3E00h  ;Close the handle
    ;int 21h
    ;return

;.l1: db "TESTLABEL",0
    lea rdx, .l1
    mov eax, 1600h  ;Create FCB
    int 21h
    return
.l1:
    istruc exFcb
    at exFcb.extSig,    db -1   ;Indicate extended FCB
    at exFcb.attribute, db dirVolumeID
    at exFcb.driveNum,  db 0    ;Current drive
    at exFcb.filename,  db "TESTLABE"
    at exFcb.fileext,   db "L  "
    at exFcb.curBlock,  dd 0
    iend 
rmlbl:
    lea rdx, .l1
    mov eax, 1300h  ;FCB delete (nice test to see if it works)
    int 21h
    return
.l1:    ;FCB to volume label
    istruc exFcb
    at exFcb.extSig,    db -1   ;Indicate extended FCB
    at exFcb.attribute, db dirVolumeID
    at exFcb.driveNum,  db 0    ;Current drive
    at exFcb.filename,  db "????????"
    at exFcb.fileext,   db "???"
    at exFcb.curBlock,  dd 0
    iend 

rnlbl:
    lea rdx, .l1
    mov eax, 1700h
    int 21h
    return
.l1:
    istruc exRenFcb
    at exRenFcb.extSig,     db -1
    at exRenFcb.attribute,  db dirVolumeID
    at exRenFcb.driveNum,   db 0    ;Current drive
    at exRenFcb.filename,   db "????????"
    at exRenFcb.fileext,    db "???"
    at exRenFcb.newName,    db "TESTLABE"
    at exRenFcb.newExt,     db "L2 "
    iend
volume:
;Only one argument. VOL X: (you can have more trash after the colon)
    test byte [arg2Flg], -1
    jnz badArgError
    test byte [arg1Flg], -1
    jnz .notCurrentDrive
    call getCurrentDrive    ;Get 0-based current drive number in al
    jmp short .dirEP
.notCurrentDrive:
    call buildCommandPath   ;Get the first argument into the searchSpec
    lea rsi, searchSpec
    call skipDelimiters     ;Move rsi to the first char of the command
    lodsw                   ;Get this word
    cmp ah, ":"             ;If this is not a colon, not a drive, error
    jne badArgError
    mov al, byte [arg1FCBret]   ;Get the response from the parse
    test al, -1
    jnz badDriveError ;Can't have either wildcards nor be invalid (obviously)
    movzx eax, byte [r8 + fcb1 + fcb.driveNum] ;Get the 1-based drive number
    dec eax ;Convert to 0 based number
.dirEP: ;Must be called with VALID 0 based drive number in al  
    call setDTA     ;Ensure we have our DTA set correctly, preserving all regs
    lea rdx, volFcb
    inc eax ;Get 1 based drive number
    mov ebx, eax    ;Save the drive number in ebx
    mov byte [rdx + exFcb.driveNum], al ;Store drive number we are 
    mov eax, 1100h ;Find first FCB
    int 21h
    push rax
    ;Print the label intro spiel, using the given (valid) drive number
    lea rdx, crlf
    mov ah, 09h
    int 21h
    lea rdx, volMes
    mov ah, 09h
    int 21h
    mov dl, bl  ;Get the 1 based drive number
    add dl, "@" ;Convert to a ASCII char to print
    mov ah, 02h
    int 21h
    pop rax
    test al, al ;If this is zero, the call succeeded
    jz .volIDOk
    lea rdx, volNo
    jmp short .volEndPrint
.volIDOk:
    lea rdx, volOk
    mov ah, 09h
    int 21h
    mov byte [cmdFFBlock + exFcb.curBlock], "$"   ;Place a $ at the end of the name
    lea rdx, qword [cmdFFBlock + exFcb.filename]
.volEndPrint:
    mov ah, 09h
    int 21h
    lea rdx, crlf
    mov ah, 09h
    int 21h
    return

version:
    lea rdx, crlf
    mov ah, 09h
    int 21h
    lea rdx, dosVer
    mov ah, 09h
    int 21h
    call .printVersionNumber
    lea rdx, crlf
    mov ah, 09h
    int 21h
    return
.printVersionNumber:
    mov ah, 30h ;Get version numbers, al = Major, ah = Minor
    int 21h
    push rax
    movzx eax, al
    call printDecimalWord
    mov dl, "."
    mov ah, 02h
    int 21h
    pop rax
    movzx eax, ah
    call printDecimalWord
    return


memory:
    mov rbx, qword [sysVars]
    test rbx, rbx
    jnz .sysvarsOK
    lea rdx, memBad0
    mov ah, 09h
    int 21h
    jmp freezePC.altEP
.sysvarsOK:
    ;Use rsi to store DOS memory, rdi to store Free memory and rbp for Hole
    ; and rcx to store Application memory
    xor esi, esi
    xor edi, edi
    xor ebp, ebp
    xor ecx, ecx
    lea rbx, qword [rbx - 8]    ;Go back a qword
    mov rbx, qword [rbx]
.memLp:
    cmp byte [rbx], mcbMarkCtn
    je .validMCB
    cmp byte [rbx], mcbMarkEnd
    jne .badMCBFound
.validMCB:
    mov eax, dword [rbx + mcb.blockSize]
    shl rax, 4  ;Convert to bytes
    cmp qword [rbx + mcb.owner], mcbOwnerDOS
    jne .notDOS
    add rsi, rax    ;Add to DOS count
    jmp short .gotoNext
.notDOS:
    cmp qword [rbx + mcb.owner], mcbOwnerFree
    jne .notFree
    add rdi, rax    ;Add to Free space count
    jmp short .gotoNext
.notFree:
    cmp qword [rbx + mcb.owner], mcbOwnerHole
    jne .notHole
    add rbp, rax    ;Add to Hole count
    jmp short .gotoNext
.notHole:
    add rcx, rax    ;Add to Application count
.gotoNext:
    cmp byte [rbx], mcbMarkEnd
    je .endOfWalk
    lea rbx, qword [rbx + mcb.program + rax]
    jmp short .memLp
.endOfWalk:
    
    lea rdx, memDOS
    mov ah, 09h
    int 21h
    mov rax, rsi
    call .mcbPrintAmount
    lea rdx, memByte
    mov ah, 09h
    int 21h

    test rbp, rbp
    jz .skipHole
    lea rdx, memHole
    mov ah, 09h
    int 21h
    mov rax, rbp
    call .mcbPrintAmount
    lea rdx, memByte
    mov ah, 09h
    int 21h
.skipHole:

    lea rdx, memApp
    mov ah, 09h
    int 21h
    mov rax, rcx
    call .mcbPrintAmount
    lea rdx, memByte
    mov ah, 09h
    int 21h

    lea rdx, memFree
    mov ah, 09h
    int 21h
    mov rax, rdi
    call .mcbPrintAmount
    lea rdx, memByte
    mov ah, 09h
    int 21h

    lea rdx, memSys
    mov ah, 09h
    int 21h
    mov rax, rsi
    add rax, rdi
    add rax, rcx
    add rax, rbp
    call .mcbPrintAmount
    lea rdx, memByte
    mov ah, 09h
    int 21h

    lea rdx, crlf
    mov ah, 09h
    int 21h
    return

.mcbPrintAmount:
    push rcx
    push rsi
    push rdi
    push rbp
    call printDecimalWord
    pop rbp
    pop rdi
    pop rsi
    pop rcx
    return
.badMCBFound:
    lea rdx, memBad2
    mov ah, 09h
    int 21h
    jmp freezePC.altEP

type:
    test byte [arg1Flg], -1 ;If this not set, error
    jz badArgError
    test byte [arg2Flg], -1
    jnz badArgError         ;If this set, error
    mov r8, [pspPtr]
    lea rsi, qword [r8 + cmdLine]
    movzx eax, byte [arg1Off]
    add rsi, rax    ;Point rsi to this argument
    cmp byte [rsi], CR
    je badArgError
    cmp byte [rsi + 1], ":" ;If a drive is specified, check if valid
    jne .noDrive
    movzx eax, byte [arg1FCBret]
    cmp al, -1
    je badDriveError
.noDrive:
    ;Now we open the provided file
    call buildCommandPath
    lea rdx, searchSpec
    mov eax, 3D00h  ;Open in read only mode
    int 21h
    jc badFileError
    lea rdx, qword [r8 + psp.dta]
    movzx ebx, ax    ;Save the file handle in ebx
.lp:
    mov ecx, 128    ;Read 128 bytes at a time
    mov ah, 3Fh ;Read handle
    int 21h
    mov ecx, eax
    jecxz .exit
    push rbx    ;Save the original in handle
    mov ebx, 1  ;STDOUT
    mov ah, 40h
    int 21h
    pop rbx ;Get back the original read handle
    jc .exitBad
    cmp eax, ecx
    je .lp
    dec ecx ;One less for a ^Z
    cmp eax, ecx
    jne .exitBad
.exit:
    mov ah, 3Eh ;Close handle
    int 21h
    return
.exitBad:
    ;Print a disk error message... use a table to build the message but for
    ; now, just exit
    ;If it is a char device, don't print a error
    jmp short .exit

exit:
    test byte [permaSwitch], -1
    retnz   ;Return if the flag is set
    mov rax, qword [realParent] ;Get actual parent...
    cmp rax, -1
    rete    ;If the real parent is -1 => Original Command Interpreter.
    mov qword [r8 + psp.parentPtr], rax ;and restore parent pointer

    mov rdx, qword [parentInt22]
    mov qword [r8 + psp.oldInt22h], rdx
    mov eax, 2522h
    int 21h

    mov eax, 4C00h  ;Exit now okay
    int 21h
    return  ;If the exit wasn't successful for some reason, return as normal

launchChild:
    ;We execute an external command here.
    ;Here we will behave like COMMAND.COM for later DOS and honour extensions.
    ;COMMAND.COM on DOS 3.3 doesn't honour the extension. If there exists a 
    ;foo.com and a foo.exe in the same dir and you type foo.exe it launches 
    ;foo.com. We will not honour this behaviour as this kinda sucks!
    call setDTA ;Searching setting
    ;Start by rebuilding the cmdFcb from the last path componant.
    lea rdi, cmdPathSpec
    call findLastPathComponant  ;Point rdi to last path componant
    mov rsi, rdi    ;Source here
    lea rdi, cmdFcb
    mov eax, 2901h  ;Skip leading blanks, clean the FCB name
    int 21h
    test al, al     ;Don't allow any wildcards in the name
    jnz badCmdError
    lea rsi, qword [cmdFcb + fcb.fileext]
    lea rdi, extStr
    mov edx, 3  ;Number of valid extension types
.extLp:
    push rsi    ;Save the ptr to the head of the file extension
    mov ecx, 3  ;Number of chars per extension
    rep cmpsb   ;Compare the two strings
    pop rsi
    test ecx, ecx
    jz .extFnd      ;If all three chars were equal, we have valid ext!
    add rdi, rcx    ;Add the remaining chars to rdi
    dec edx         ;Else one less extension type to check
    jnz .extLp
    ;None of the three extensions were ok, so check if it is all spaces.
    ;If not, error.
    mov ecx, 3
    mov rdi, rsi
    mov al, SPC
    rep scasb   
    test ecx, ecx   ;Are all three chars spaces?
    jnz badCmdError    ;If not, error!
    ;Now we search first with COM, then EXE, then BAT. 
    lea rsi, extStr
    mov ebx, 3  ;Use ebx as the attempt counter
.extSrchLp:
    lea rdi, qword [cmdFcb + fcb.fileext]
    mov ecx, 3
    rep movsb   ;Copy the extension over!
    call .prepAndSearch     ;Prep and search path in rdx.
    jnc .extSrchFnd         ;If CF=NC, find found!
    dec ebx                 ;Decrement extension counter
    jnz .extSrchLp
;Here we have ran out of extensions to search for! Now if the path was rel
; we start prepending path componants and trying again. To do this, we reset
; by setting the fcb extension back to all spaces.
    lea rdi, qword [cmdFcb + fcb.fileext]
    mov ecx, 3
    mov al, SPC
    rep stosb   ;Store back the empty extension!
    jmp short .pathHandle
.extFnd:
;Here if the file had the right extension.
    call .prepAndSearch    ;Prep and search path in rdx.
    jc .pathHandle
.extSrchFnd:
;Pathspec in rdx exists, so now we prepare to launch it! First check it is not
; a BAT. If it is, separate handling!
    cmp byte [cmdFcb + fcb.fileext], "B"    ;If it is B, its a batch!
    je .batLaunch
    mov r8, qword [pspPtr]
    lea rbx, launchBlock
    mov rax, qword [r8 + psp.envPtr]    ;Get the env pointer
    mov qword [rbx + execProg.pEnv], rax 
    lea rax, qword [r8 + cmdTail]
    mov qword [rbx + execProg.pCmdLine], rax
    lea rax, qword [r8 + fcb1]
    mov qword [rbx + execProg.pfcb1], rax
    lea rax, qword [r8 + fcb2]
    mov qword [rbx + execProg.pfcb2], rax
    lea rdx, cmdPathSpec
    mov eax, 4B00h  ;Load and execute!
    int 21h
.pathHandle:        ;For now just place it here! Will fix later!
    jmp badCmdError    ;If something goes wrong, error out

.batLaunch:
    lea rdx, .batMsg
    mov eax, 0900h
    int 21h
    return
.batMsg db "BATCH preprocessor not implemented",CR,LF,"$"

.prepAndSearch:
;Copies over the name and extension in UC to the last componant of the 
;cmdPathSpec and null terminates. 
;Input: cmdFcb name.ext setup. 
;Output: rdx -> Filled in cmdPathSpec 
;        CF=NC, file in rdx found. CF=CY, file in rdx not found!
    push rax
    push rcx
    push rsi
    push rdi
    lea rdi, cmdPathSpec 
    mov rdx, rdi    ;Save the path ptr in rdx
    call findLastPathComponant  ;Point rdi to the final path componant 
    lea rsi, qword [cmdFcb + fcb.filename]
    call FCBToAsciiz    ;Get an asciiz suffix
    mov eax, 4E00h  ;Find first
    mov ecx, dirSystem  ;Normal, RO and System files only searchable!
    int 21h
    pop rdi
    pop rsi
    pop rcx
    pop rax
    return