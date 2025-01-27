;Functions included with name and label:
;----------------------------------------------------
; LABEL     NAME
;----------------------------------------------------
; dir       DIR
; chdir     CHDIR(CD)
; mkdir     MKDIR(MD)
; rmdir     RMDIR(RD)
; copy      COPY
; erase     DEL(ERASE)
; date      DATE
; time      TIME
; ctty      CTTY
; cls       CLS
; break     BREAK
; verify    VERIFY
; rename    REN(AME)
; truename  TRUENAME
; volume    VOL
; version   VER
; memory    MEM
;----------------------------------------------------
dir:
;Don't allow for searching unmounted network drives... is this a limitation?
    mov byte [dirFlags], 0    ;Clear DIR flags
    mov byte [dirLineCtr], 0
    mov dword [dirFileCtr], 0
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
    test dword [dirFileCtr], -1
    jnz .filesFound
    ;Else print File not found and exit!
    lea rdx, fnfMsg
    call printString
    return
.filesFound:
    mov eax, dword [dirFileCtr]   ;Get number of files
    mov ecx, 9
    call printDecimalValLB
    lea rdx, dirOk
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
    mov ecx, 17     ;2 Tb with commas needs this 
    call printDecimalValLB
    lea rdx, bytesOk
    mov ah, 09h
    int 21h
    return

.dirPrintFileData:
;Use cmdFcb to build the file name with space
;Start by print the name (same for both cases)
;We first check if the file has attributes hidden/system and hide them if so
    test byte [cmdFFBlock + ffBlock.attribFnd], dirIncFiles | dirCharDev
    retnz   ;Simply return if either bit is set
    lea rsi, qword [cmdFFBlock + ffBlock.asciizName]
    lea rdi, cmdFcb
    call asciiFilenameToFCB
    lea rdx, cmdFcb
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
    mov ah, 09h
    int 21h
    mov dl, SPC
    mov ah, 02h
    int 21h
    jmp short .dirPrintFileDT
.dirPrintNotDir:
;Here we print the file size
    mov dl, " "
    mov ah, 02h
    int 21h
    mov eax, dword [cmdFFBlock + ffBlock.fileSize]
    mov ecx, 13
    call printDecimalValLB
    mov dl, SPC
    mov ah, 02h
    int 21h
.dirPrintFileDT:
    movzx eax, word [cmdFFBlock + ffBlock.fileDate]
    xor ebx, ebx    ;Ensure we print 2 digit year
    call printDate
    lea rdx, threeSpc
    mov ah, 09h
    int 21h
    movzx eax, word [cmdFFBlock + ffBlock.fileTime]
    call printTime
    lea rdx, crlf
    mov ah, 09h
    int 21h
    mov al, 23  ;23 lines... for the next bit
    jmp short .dirPrintNameExit
.widePrint:
;If /W, print name space ext space space space space
    lea rdx, fourSpc
    mov ah, 09h ;Print string
    int 21h
    mov al, 5*23    ;5 entries per line...
.dirPrintNameExit:
    inc dword [dirFileCtr]   ;Increment file counter
    inc byte [dirLineCtr]
    cmp byte [dirLineCtr], al
    retne
    test byte [dirFlags], dirPageType
    jz .dirPrnNoPag
    lea rdx, pauseMes
    mov ah, 09h
    int 21h
    mov ah, 01h ;Wait for a char from STDIN
    int 21h
    lea rdx, crlf   ;Force new line
    mov ah, 09h
    int 21h
.dirPrnNoPag:
    mov byte [dirLineCtr], 0
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
    jmp printString

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
    jmp printString

copy:
    mov word [sourceHdl], -1
    mov word [destHdl], -1
    mov word [srcHdlInfo], -1
    mov byte [bCpFlg], 0    ;State flag!
    mov qword [cpBufPtr], 0 ;Init to null ptr!
    mov word [wCpBufSz], 0  ;Clear buffer count        
    mov dword [dCpCnt], 0   ;Clear file count
    mov byte [srcSpec], 0   ;Clear first bytes of the buffers
    mov byte [destSpec], 0
    mov qword [srcPtr], 0
    mov qword [destPtr], 0
    mov eax, 5400h  ;Get verify flag
    int 21h
    mov byte [verifyFlg], al    ;Save verify flag!
;Start with getting a disk buffer, of the size of the internal disk buffers. 
;If we cant allocate full diskbuffer, get as much as we can
;Diskbuffer as thats the best optimisation for IO buffers
    mov eax, 5200h
    int 21h ;Get in rbx ptr to sysvars
    movzx ecx, word [rbx + 20h] ;Get the internal buffer size in ecx!
    mov ebx, ecx
    shr ebx, 4  ;Convert to paragraphs, save bytes in ecx
    mov eax, 4800h
    int 21h
    jnc .bufOk
    test ebx, ebx       ;Cannot allocate? Yikes...
    jnz .okSize
.badAlloc:
;Not enough memory error!
    lea rdx, cpNoMem
    jmp badCmn  ;Print the string and return!
.okSize:
    mov ecx, ebx    
    shl ecx, 4  ;Convert into bytes from paragraphs
    mov eax, 4800h
    int 21h
    jc .badAlloc
.bufOk:
    mov qword [cpBufPtr], rax   ;Save ptr to xfr area
    mov word [wCpBufSz], cx     ;Save buffer size
    call copyParse      ;Do Mode 3 prescan
    jc badSyntaxError
    test ecx, ecx   
    jz badSyntaxError
;Go to the end of the cmd line and search backwards for the destination first
    lea rsi, qword [r8 + cmdLine]
    movzx ecx, byte [r8 + cmdLineCnt]
    dec ecx ;Turn into offset
    add rsi, rcx
    xor edx, edx    ;Use edx as a counter for number of destination switches
    std ;Go in reverse
.sd:    ;At this point, any switch chars affect destination!
    call skipDelimiters ;skipDelimiters in reverse!
    mov al, byte [switchChar]
    cmp byte [rsi + 1], al  ;Did we hit a switch?
    jne .noSwitch
    ;Here we hit a switchchar! Process it! rsi points to char before switchchar
    movzx eax, byte [rsi + 2]   ;Get the char
    push rax        ;Push the switchchar
    inc edx         ;Inc the counter
    jmp short .sd   ;Now go back to skipping delimiters again!
.noSwitch:
;Ok so we hit a path. Now set the flags based on the stack!
    test edx, edx
    jz .noDestSwitch
.ns1:
    pop rax ;Pop the switch char
    mov bl, ascDes
    call .doSwitchRev
    jnz .badExit    ;Invalid switch, abort procedure!    
    dec edx
    jnz .ns1
.noDestSwitch:
;Now search for the starting delimiter or start of line
    inc rsi ;Go to the last char in the path
.noSwitchLp:
    lodsb   ;Get char at rsi, go back a char
    call isALdelimiter
    je .destFnd
    mov al, byte [switchChar]
    cmp byte [rsi - 1], al  ;Peek if an embedded switch?
    je .se  ;Jump if so!
    dec ecx ;One less char left to search
    jmp short .noSwitchLp
.se:
    dec rsi     ;Dec to make the below work!
.destFnd:
    add rsi, 2  ;Go back to the first char past the delimiter
    cld 
    mov rbp, rsi    ;Save this ptr in rbp temporarily
    lea rdi, destSpec
    push rdi
    call cpDelimPathToBufz ;Copy this over!   
    pop rdi
    call findLastPathComponant  ;Get last path componant in rdi
    mov qword [destPtr], rdi
    cmp byte [rdi + 1], ":" ;Is this a colon?
    jne .destEnd    ;Dont worry
    mov al, byte [rdi]      ;Pick up drive letter
    call ucChar             ;Make sure we UC the drive letter
    mov byte [rdi], al      ;Store the drive letter
    add rdi, 2              ;Point to this null
    mov qword [destPtr], rdi    ;Store chars over the null
.destEnd:
;Now start with source processing!! Clear the binSrc bit in bCpFlg
    and byte [bCpFlg], ~binSrc  ;Mightve been accidentally set by dest flags
    lea rsi, qword [r8 + cmdLine]
.srcLp: ;Jump to with rsi pointing to portion of the cmdline to scan
    call skipDelimiters 
    mov al, byte [switchChar]
    cmp byte [rsi], al
    jne .noSrcSw    ;Not switch! Must be filename start!
    ;rsi points to switch after switchchar
    mov bl, ascSrc
    call .doSwitch
    jnz .badExit
    add rsi, 2
    jmp short .srcLp    ;Now keep searching for start of filename
.noSrcSw:
    cmp rbp, rsi
    jne .noSameSrcDest
    ;Here if the destination we specified earlier is the same as the source
    ;This means, the actual destination is the current default drive
    ; on the current directory. Pass to DOS X:FILENAME.EXT<NUL> string.
    call getCurrentDrive    ;Get current drive
    add al, "A"             ;Turn into a drive letter
    mov ah, ":"
    movzx eax, ax
    mov dword [destSpec], eax   ;Add a default null terminator
    lea rdi, destSpec+2         ;Point to the null after the colon
    mov qword [destPtr], rdi    ;Copy the filenames here
    or byte [bCpFlg], mod1Cpy   ;Copy to curdir in cur drive
.noSameSrcDest:
    lea rdi, srcSpec            ;rsi now goes into the source spec!
    push rdi
    call cpDelimPathToBufz      ;Copy this over! rsi points past delimiter
    pop rdi
    ;Now go forwards and pick up any more switches.
    dec rsi ;Point back to the first delimiter
.swSrcSwPost:
    call skipDelimiters ;Skips trailing delimiters
    mov al, byte [switchChar]
    cmp byte [rsi], al
    jne .swSrcSwPostExit
    ;rsi points to switch after switchchar
    mov bl, ascSrc
    call .doSwitch  ;Puts us at the char past the switch itself
    jnz .badExit
    add rsi, 2      ;Go past the switch
    jmp short .swSrcSwPost
.swSrcSwPostExit:
    mov rsi, rdi
    call scanForWildcards   ;Find if we have a WC in this source!
    jnz .oneCp
    or byte [bCpFlg], wcSrc ;We are copying many files. Disp names
.oneCp:
    call findLastPathComponant  ;Get last componant of src in rdi
    mov qword [srcPtr], rdi     ;Now save the last componant
    cmp byte [rdi + 1], ":"     ;Is this a colon?
    jne .srcEnd             ;Dont worry
    mov al, byte [rdi]      ;Pick up drive letter
    call ucChar             ;Make sure we UC the drive letter
    mov byte [rdi], al      ;Store the drive letter
    add rdi, 2              ;Point after the colon
    mov qword [srcPtr], rdi ;Store chars past the colon
.srcEnd:
;Now uppercase both paths
    lea rsi, srcSpec
    call normalisePath
    lea rdi, destSpec
    call normalisePath
    push rdi
    test byte [bCpFlg], mod3Cpy
    jz .notm3
;Now we scan for a + in the src spc and if one is found, overwrite it with a <NUL>
    mov rdi, rsi    ;Get the src string in rdi
    call strlen 
    mov al, "+"
    repne scasb 
    jne .notm3  ;If not equal, exit
    mov byte [rdi - 1], 0
.notm3:
;Now establish if the source is a directory or not!
    pop rdi ;Get back the destspec ptr
    test byte [bCpFlg], wcSrc
    jnz .checkDestDir   ;Skip check if source has wildcards
    lea rdx, srcSpec
    mov rsi, rdx
    lodsw   ;Get the first two chars, is it "X:" style
    cmp ah, ":"
    jne .isSrcDir   ;If not, check explicitly
    lodsb           ;Get char past :
    test al, al     ;Is byte three nul?
    jz .srcStorWc   ;If so, we have "X:<NUL>"
    cmp al, byte [pathSep]  ;If char past : isnt pathsep, check manually
    jne .isSrcDir
    lodsb           ;Get char past char past "X:\"
    test al, al     
    jnz .isSrcDir   ;Fall thru is "X:\<NUL>"
.srcStorWc:
;rsi points past the char to store the WC at
    mov dword [rsi - 1], "*.*"  ;Store with terminating null!
    or byte [bCpFlg], wcSrc     ;We're adding the wildcard to the source!
    jmp short .checkDestDir
.isSrcDir:
    mov ecx, dirDirectory
    mov eax, 4E00h
    int 21h
    jc .checkDestDir    ;Wasn't a dir!
    ;Was the file we found actually a directory?
    cmp byte [cmdFFBlock + ffBlock.attribFnd], dirDirectory
    jne .checkDestDir   ;Wasn't a dir
    mov rdi, rdx
    call strlen
    dec ecx
    add rdi, rcx    ;Move rdi to the terminating null
    mov al, byte [pathSep]
    cmp byte [rdi - 1], al  ;Do we have a trailing pathsep?
    je .srctpsp
    stosb   ;Store the pathsep over the null, inc rdi
.srctpsp:
    mov qword [srcPtr], rdi ;Update the srcPtr
    mov eax, "*.*"
    stosd   ;Store the WC with terminating nul!
    or byte [bCpFlg], wcSrc ;We're adding the wildcard to the source!
.checkDestDir:
;Now establish if destination is a directory or not!
    test byte [bCpFlg], mod1Cpy ;If we already know its mod1, skip
    jnz .mod1
    lea rsi, destSpec
    lodsw   ;Get the first word, i.e. candidate "X:"
    cmp ah, ":"
    jne .isDestDir
    lodsw   ;Get second word, i.e. candidate "\<NUL>" or "<NUL>"
    test al, al ;Is byte three nul?
    jz .mod1    ;If so, it was a X:<NUL>
    test ah, ah ;Is byte four nul?
    jnz .isDestDir  ;If not, check if destination is a directory
    cmp al, byte [pathSep]  ;Else, check if byte 3 was a pathsep!
    je .mod1    ;If it is, then it was a X:\<NUL>
.isDestDir:
;Only here if we suspect the destination pathspec is a path, not file!
    lea rdx, destSpec
    mov ecx, dirDirectory
    mov eax, 4E00h
    int 21h
    jc .mod2
    ;Was the file we found actually a directory?
    cmp byte [cmdFFBlock + ffBlock.attribFnd], dirDirectory
    jne .mod2
    ;Here we just check if we have a terminating slash on the destination path.
    ;If not, we need to place one there!
    mov rdi, rdx    ;Move destSpec
    call strlen
    dec ecx         ;Lose the null terminator, zero upper bits
    add rdi, rcx    ;Go to null terminator
    movzx eax, byte [pathSep]
    cmp byte [rdi - 1], al  ;Is the char before the null a pathsep?
    je .mod1        ;If so, skip adding another one!
    stosw           ;Else, store the pathsep and null!
    dec rdi         ;Point back to null
    mov qword [destPtr], rdi    ;Save this pointer
    jmp .mod1
.mod2:
;Here we are copying files(s) to file(s)! Filenames are copied according 
; to rename wildcard semantics. Always run through this as the destination
; name may have wildcards in it! But we don't add any.
    or byte [bCpFlg], mod2Cpy   ;Set to remind us what we are doing!
    lea rsi, destSpec
    call scanForWildcards   ;ZF=ZE if WC cound
    jz .m2Wc
    or byte [bCpFlg], oneDest   ;The destination is a single file!
.m2Wc:
;Search for the source file
    lea rdx, srcSpec
    xor ecx, ecx    ;Normal and RO files pls
    mov eax, 4E00h
    int 21h
    jc .badSrcFile
    mov rsi, qword [destPtr]
    lea rdi, qword [r8 + fcb2]  ;Create the permanent destination pattern
    mov eax, 2901h
    int 21h
.md2Lp:
    lea rsi, cmdFFBlock + ffBlock.asciizName
    lea rdi, qword [r8 + fcb1]  ;Create the source pattern
    mov eax, 2901h
    int 21h  
;Now depending on source and dest patterns, build a filename in renName
    lea rsi, qword [r8 + fcb2 + fcb.filename]    
    lea rdi, renName    ;Start by copying the destination pattern
    push rdi
    movsq
    movsw
    movsb
    pop rdi
    lea rsi, qword [r8 + fcb1 + fcb.filename] ;Now start sourcing chars!
    mov ecx, 11 ;11 chars to copy!
.md2NameMake:
    lodsb   ;Get the char from the source string
    cmp byte [rdi], "?" ;Are we over a WC?
    jne .noStore    ;Dont store the char there
    mov byte [rdi], al  ;If we are over a wc, store the char there!
.noStore:
    inc rdi ;Goto next char position
    dec ecx
    jnz .md2NameMake
    lea rsi, cmdFFBlock + ffBlock.asciizName
    mov rdi, qword [srcPtr]
    call strcpy ;Doesn't matter that we preserve the pointers
    lea rsi, renName
    mov rdi, qword [destPtr]
    call FCBToAsciiz
    call .prntFilespec  ;Prints the source filename
    call copyMain       ;And copy it!
    jnc .md2Ok
;Errors are not ignored, only when overwrite on ASCII concat mode, in which case
; we display a "contents lost" message and proceed. Here all errors halt!
    cmp al, -3
    je .badBinCdevErr
    cmp al, -2
    je .rootDirFull
    cmp al, -1      ;Source and destination same?
    je .badSameFile 
    jmp .badExit    ;Else generic error message
.md2Ok:
    test byte [bCpFlg], wcSrc   ;If no Wildcards in source, we are done!
    jz .copyDone        ;Copy complete!
    mov eax, 4F00h      ;Else, find Next File
    int 21h
    jc .copyDone        ;If no more files, we are done!
    jmp .md2Lp    ;Else, now build a new source and destination!
.mod1:
;Here we are copying file(s) into a directory. Filenames are copied verbatum.
    or byte [bCpFlg], mod1Cpy   ;Ensure this bit is set!
    lea rdx, srcSpec
    xor ecx, ecx    ;Normal and read only files pls!!
    mov eax, 4E00h
    int 21h
    jc .badSrcFile  ;File not found error!!
.mod1Lp:
    lea rsi, cmdFFBlock + ffBlock.asciizName
    mov rdi, qword [srcPtr]
    call strcpy2    ;Place the asciiz name at the end of the path
    mov rdi, qword [destPtr]
    call strcpy2    ;Place the asciiz name at the end of the path
    call .prntFilespec
    call copyMain   ;And copy it!
    jnc .mod1Ok
;Errors are not ignored, only when overwrite on ASCII concat mode, in which case
; we display a "contents lost" message and proceed. Here all errors halt!
    cmp al, -3
    je .badBinCdevErr
    cmp al, -2
    je .rootDirFull
    cmp al, -1      ;Source and destination same?
    je .badSameFile 
    jmp .badExit    ;Else generic error message
.mod1Ok:
    test byte [bCpFlg], wcSrc   ;If no Wildcards in source, we are done!
    jz .copyDone
    mov eax, 4F00h      ;Find Next File
    int 21h
    jnc .mod1Lp         ;If no more files, we are done! Fall thru!

.copyDone:
    test byte [bCpFlg], mod3Cpy ;If not mode 3, just exit!
    jz .cdNotM3
;Else, we advance the pointers and jump again!
    mov rsi, qword [pNextFspec]
    cmp rsi, qword [pLastFspec] ;Did we check the last section?
    je .cdNotM3                 ;If yes, we are done!
.cdM3lp:
    call copyParse.gotoVar      ;Else move rsi to the var
    cmp al, CR
    je .cdNotM3
    cmp al, "+"
    je .cdPlus
    call copyParse.skipVar  ;Go to the end of the var (guaranteed @ a +)
    cmp al, CR
    je .cdNotM3
    jmp short .cdM3lp
.cdPlus:
    inc rsi ;Go past the + sign
    mov qword [pNextFspec], rsi ;Store this ptr
    and byte [bCpFlg], mod3Cpy|oneDest|ascDes   ;Clear flags except 4 dest
    jmp .srcLp
.cdNotM3:
    call .copyCleanup   ;Clean up resources!
    mov eax, dword [dCpCnt] ;Get number of files copied
    mov ecx, 9  ;Maximum copy 9,999,999 files... ofc thats ok
    call printDecimalValLB   ;n File(s) copied
    lea rdx, copyOk
    mov eax, 0900h
    int 21h    
    return

.prntFilespec:
;Prints the filespec to STDOUT. If the path is 
    test byte [bCpFlg], wcSrc|mod3Cpy   ;If no wc or mode 3 don't print name
    retz
    lea rdx, srcSpec
    mov rdi, rdx
    call strlen
    dec ecx     ;Drop terminating null
    mov ebx, 1  ;STDOUT
    mov eax, 4000h  ;Write
    int 21h
    call printCRLF
    return
.doSwitch:
;Since switches can come before or after a name, handle them here!
;If invalid switch char, returns ZF=NZ.
;Input: bl = ASCII bit to set (either 1 or 2) 
;       rsi -> Switchchar
    push rsi
    inc rsi ;Point to char past switchchar
    lodsb   ;Get this char, goto next char
    pop rsi
.doSwitchRev:
    call ucChar
    cmp al, "A"
    jne .cB
    or byte [bCpFlg], bl    ;Set the ASCII bit
    and byte [bCpFlg], ~binSrc  ;ASCII flag clears this
.cExit:
    xor ebx, ebx    ;Clear ZF 
    return
.cB:
    cmp al, "B"
    jne .cV
    not bl  ;Reverse bits
    and byte [bCpFlg], bl   ;Clear the ASCII bit.
    or byte [bCpFlg], binSrc    ;This was explicitly set
    jmp short .cExit
.cV:
    cmp al, "V"
    retne   ;Exit w/o clearing ZF
    test byte [verifyFlg], -1   ;If verify flag set, do nothing
    jnz .cExit                  ;If not zero, flag already set!
    ;Else, set it. We return it at the end!
    mov eax, 2E01h  ;Set Verify Flag
    int 21h
    jmp short .cExit

;COPY Bad Exits!!
.badBinCdevErr:
    lea rdx, binDevErr
    jmp short .badExitCmn
.rootDirFull:
    lea rdx, fulRootDir
    jmp short .badExitCmn
.badSameFile:
    lea rdx, noSelfCopy
.badExitCmn:
    call badCmn     ;Print error message
    jmp .copyDone   ;Clean resources
.badSrcFile:
    call badFnf  ;File not found!!
    jmp .copyDone
.badExit:
    call badParamError
    jmp .copyDone

.copyCleanup:
;Clean all resources!! Reset verify and free copy buffer. 
;Handles are never open in this process!
    mov eax, 2E00h
    mov al, byte [verifyFlg]
    int 21h
    push r8
    mov r8, qword [cpBufPtr]
    test r8, r8 ;Check zero, clear CF
    jz .skipFree
    mov eax, 4900h
    int 21h
.skipFree:
    pop r8
    jc freezePC ;If free fails, man....
    return

copyMain:
;This is the main copying procedure! 
;Start by checking the two files are not the same. If so, complain!
;If returns CF=CY, error code in al. 
;   If al = -1, same filename error!
;   If al = -2, Root Dir full (couldn't create file)
;   If al = -3, attempted binary read from a device
;If returns CF=NC, file copied successfully.
;Check the two files are not the same using truename in searchspec
    lea rsi, srcSpec
    lea rdi, searchSpec
    mov eax, 6000h  ;TRUENAME
    int 21h 
    push rdi    ;Save this searchSpec
    lea rsi, destSpec
    lea rdi, searchSpec + cmdBufferL ;Use the latter half to store this bit
    mov eax, 6000h
    int 21h
    pop rsi     ;Get this ptr back
    mov eax, 121Eh  ;Cmpr ASCII strings
    int 2Fh
    jnz .notSameFile
;Now we check if we are in a concat mode. 
; If in Concat and we are on the first file, just move the file pointer 
; to the end of the file and return.
; If in Concat and we are not on the first file, print content of 
; destination lost and return.
; Else, exist bad.
;Concat is defined as (mod3Cpy or wcSrc) and oneDest
    movzx eax, byte [bCpFlg]   ;Get the flags
    mov ecx, eax        ;Copy the flags
    and ecx, oneDest    ;Isolate for AND
    jz .sameNameExit    ;If not set, not in Concat mode
    and eax, mod3Cpy | wcSrc   ;Compute the OR
    mov ecx, eax
    and eax, mod3Cpy    ;Save this bit
    and ecx, wcSrc      ;And this bit
    or eax, ecx         ;Construct a single vale.
    jz .sameNameExit    ;If neither bit was set, we don't have condition.
    lea rdx, destSpec   ;Prepare rdx to the destination
    test dword [dCpCnt], -1 ;Is this the first file?
    jnz .notFirstLost   ;If not, we skip this file from copy!
    call .openFile      ;Moving the fp suffices for the copy
    return
.notFirstLost:
    lea rdx, filLostErr
    call printString    ;Print this string and skip this file
    return
.sameNameExit:
    mov al, -1  ;Same filename error!
.badExit:
    push rax
    call .exitCleanup
    pop rax
    stc
    return
.badExitNoSpace:
    mov al, -2  ;Access denied from Create happens if Root Dir full!
    jmp short .badExit
.notSameFile:
;Open source with read permission
;Open destination with write permission
    lea rdx, srcSpec
    mov eax, 3D00h  ;Read open
    int 21h
    jc .badExit
    mov word [sourceHdl], ax
    movzx ebx, ax   ;For bx

    mov eax, 4400h  ;Get device info for file in bx in dx
    int 21h
    mov word [srcHdlInfo], dx   ;Store information here
    test dl, 80h    ;Is this a chardev?
    jz .prepCopy
;Check the binary flag was not set on this source filespec
    mov al, -3  ;Prep the error code
    test byte [bCpFlg], binSrc
    jnz .badExit    ;Return with the error code in al
    or byte [bCpFlg], ascSrc    ;Set the ascii read bit for later!
.prepCopy:
    xor esi, esi                ;Flag if ASCII copy done after write!
    mov rdx, qword [cpBufPtr]   ;Get the buffer pointer
.copyLoop:
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; CONSIDER MODIFYING THIS ROUTINE SO THAT ASCII READS ONLY WRITE AFTER       !
; FINDING A ^Z, OR FILLING THE BUFFER. CURRENTLY, WRITES HAPPEN ON EACH LINE.!
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    movzx ecx, word [wCpBufSz]
    movzx ebx, word [sourceHdl]
    mov eax, 3F00h ;Read
    int 21h
    jc .badExit
    mov ecx, eax    ;Save the binary # of bytes read
    test byte [bCpFlg], mod3Cpy
    jnz .doMod3BinCheck
    test byte [bCpFlg], ascSrc  ;Are we copying in ascii mode
    jz .notAscii
    jmp short .doAscii
.doMod3BinCheck:
    test byte [bCpFlg], binSrc
    jnz .notAscii
.doAscii:
;Now scan the buffer for a EOF. If we find, we stop the copy at that char
    test word [srcHdlInfo], 80H ;Is this a chardev?
    jnz .charDev
    jecxz .exitCleanup  ;We read no bytes from disk so can't scan for an EOF!
.charDev:
    push rax        ;Save the original char count
    mov rdi, rdx    ;rdx keeps the buffer ptr
    mov al, EOF
    repne scasb     ;Loop thru now!
    mov eax, ecx    ;Move the offset into the buffer in eax
    pop rcx         ;Get the original read count into ecx
    jne .notAscii   ;If EOF not found, ecx has the full buffer size to write
;Here if EOF found.
    inc eax         ;Drop one for the EOF char itself
    sub ecx, eax    ;Get difference for # of chars to write
    dec esi         ;Set to -1 to indicate we are done with ASCII copy!
.notAscii:
    test ecx, ecx   ;If no bytes were read, exit!
    jz .exitCleanup
    push rcx    ;Save the number of bytes to write
    push rdx    ;Save the buffer ptr
    call .getWriteHandle    ;Gets hdl in bx. Inc file ctr on create/open
    pop rdx
    pop rcx
    jc .badExitNoSpace
    mov eax, 4000h ;Write to handle in bx, to buffer in rdx
    int 21h
    jc .badExit
    cmp ecx, eax            ;ecx=bytes to write, eax=bytes written
    jne .badExitNoSpace     ;Disk must be full!!
    test esi, esi           ;Always clear in Binary mode
    jz .copyLoop            ;Set possible in ASCII mode
.exitCleanup:
;Add a terminating EOF if we have been asked to. Else, skip!
    test byte [bCpFlg], ascDes
    jz .ecNoEOF
    movsx ebx, word [destHdl]
    cmp ebx, -1 ;If this hdl is closed, skip this!
    je .ecNoEOF
    mov rdx, qword [cpBufPtr]
    mov byte [rdx], EOF
    mov ecx, 1
    mov eax, 4000h  ;Write and ignore any errors that come from this!
    int 21h
.ecNoEOF:
;Now close copy handles!
    mov bx, word [sourceHdl]
    cmp bx, -1
    je .beSkipSource
    mov eax, 3E00h  ;Close this handle
    int 21h
    mov word [sourceHdl], -1    ;Reset the var
.beSkipSource:
    mov bx, word [destHdl]
    cmp bx, -1
    rete
    mov eax, 3E00h  ;Close this one too!
    int 21h
    mov word [destHdl], -1  ;Reset the var
    return

.getWriteHandle:
;Returns in bx the handle to the destination file. If the file has not 
; yet been opened, creates/opens the destination file as appropriate.
;Input: Nothing
;Output: CF=NC: bx = Handle to the file
;        CF=CY: File failed to open/create. 
    movzx ebx, word [destHdl]
    cmp bx, -1
    je .gwHProceed
    clc     ;Clear the CF
    return
.gwHProceed:
;Here if the destination has not been opened yet!
    lea rdx, destSpec   ;Prepare rdx to the destination
;Now we create/reopen the file here!
    test byte [bCpFlg], oneDest ;If single destination, concatenate
    jz .createFile
;Now we first try to open this file. If this is file 0, we create.
;If this is more than file 0, we open
    cmp dword [dCpCnt], 0
    je .createFile
;Now we open the file instead and append to the end
.openFile:
    mov eax, 3D02h  ;Open the file in exclusive read/write mode
    int 21h
    retc
    inc dword [dCpCnt]  ;We've opened the file for writing!
    mov word [destHdl], ax
    movzx ebx, ax
    xor ecx, ecx
    xor edx, edx
    test byte [bCpFlg], ascDes  ;Did we write a terminating ^Z before?
    jz .gwHNoAscii
    dec edx                     ;Overwrite the ^Z
    dec ecx                     ;PASS A SIGNED -1 !!!!
.gwHNoAscii:
    mov eax, 4202h  ;LSEEK from the end
    int 21h
    return
.createFile:
    mov eax, 3C00h  ;Create the file
    xor ecx, ecx    ;No file attributes
    int 21h
    retc
    inc dword [dCpCnt]  ;File created for writing!
    mov word [destHdl], ax
    movzx ebx, ax
    return

copyParse:
;Checks to see if we are in mode 3. If we are, sets up the 
; mode 3 variables. Ignores switches making life so much better.
    lea rsi, qword [r8 + cmdLine]
    mov qword [pNextFspec], rsi     ;Setup this var here
    xor ecx, ecx    ;Use as a filespec counter in a "+ +" region
.cp1:
    call .gotoVar   ;Goto filespec. Move rsi->First non-terminator char or +
    cmp al, CR
    je .cpExit
    cmp al, "+"
    je .cpPlus
    inc ecx ;Add another file to the count
    call .skipVar   ;Goto end of var. Move rsi->First terminator char or +
    cmp al, CR
    je .cpExit
    cmp al, "+"
    jne .cp1 
.cpPlus:
;Here we hit a "+"
    xor ecx, ecx    ;Clean this var again
    or byte [bCpFlg], mod3Cpy   ;We are in mode 3 if we are here
    inc rsi         ;Go past the + sign
    mov qword [pLastFspec], rsi ;Save this var as we go
    jmp short .cp1
.cpExit:
    cmp ecx, 3  ;If we have more than 2 files w/o a + between them, syntax err
    cmc         ;Compliment CF to make CF=CY on error!
    return
.gotoVar:
;Goes to the next variable, plus sign or CR.
;Returns rsi -> First char of the pathspec, + or CR
    lodsb
    call isALdelimiter  
    je .gotoVar         ;Skips delimiters
    cmp al, byte [switchChar]
    jne .vexit
    call .skipVar   ;Now skip the string after the cmdline switch
    jmp short .gotoVar  ;And switches
.vexit:
    dec rsi ;Return back to the break condition char
    return
.skipVar:
;Goes to the delimiter for this string
    lodsb
    cmp al, CR
    je .vexit
    cmp al, "+"
    je .vexit
    call isALdelimiter
    je .vexit
    jmp short .skipVar


erase:
    test byte [arg1Flg], -1
    jz badArgError
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
    call printCRLF      ;Else, tell me what you want!!!
    jmp short .ynmsg    
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
    lea rsi, qword [r8 + cmdLine]
    call skipDelimiters
    cmp byte [rsi], CR  ;If nothing, get input
    jne .goDate  ;Else rsi is pointing to something possibly a date. Try it!
.init:
    ;Else, we do interactive mode!
    lea rdx, curDate
    mov ah, 09h
    int 21h
    call printFmtDate
.noCur:
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

    lea rdx, cLineBuffer  ;Make sure to use cLineBuffer to preserve F3
    mov byte [rdx], inLen ;Enter a string of up to 128 chars in length
    mov ah, 0Ah
    int 21h
    push rdx
    call printCRLF
    pop rdx
    cmp byte [rdx + 1], 0   ;If the user typed nothing...
    rete    ;Exit!
    ;No spaces anywhere, separator chars allowed are / . - only
    lea rsi, qword [rdx + 2]    ;Go to the text portion
.goDate:
    xor eax, eax    
    cmp byte [ctryData + countryStruc.dtfmt], 1
    jb .us
    ja .jpn
    ;Here for UK style dates
    call getByte
    jc .badDate
    mov byte [td3], al  ;Store the day
    call .validSep
    jne .badDate
    call getByte
    jc .badDate
    mov byte [td4], al  ;Store month
    call .validSep
    jne .badDate
    call getNum
    call .doYear    ;Adjusts the year if necessary
    cmp eax, 10000h           ;If it is too large, fail!
    jae .badDate
    mov word [td1], ax  ;Store the word directly
.writeDate:
    call skipDelimiters
    cmp byte [rsi], CR  ;Must be terminated by a CR!!
    jne .badDate
    movzx ecx, word [td1]   ;Get the year
    mov dx, word [td3]      ;Read time and date together!
    mov eax, 2B00h
    int 21h
    test al, al
    retz
.badDate:
    lea rdx, badDate
    call printString
    jmp date.noCur
.us:
    call getByte
    jc .badDate
    mov byte [td4], al  ;Store the month
    call .validSep
    jne .badDate
    call getByte
    jc .badDate
    mov byte [td3], al  ;Store day
    call .validSep
    jne .badDate
    call getNum
    call .doYear    ;Adjusts the year if necessary
    cmp eax, 10000h           ;If it is too large, fail!
    jae .badDate
    mov word [td1], ax  ;Store the word directly
.writeHop:
    jmp short .writeDate
.jpn:
    call getNum
    call .doYear    ;Adjusts the year if necessary
    cmp eax, 10000h           ;If it is too large, fail!
    jae .badDate
    mov word [td1], ax  ;Store the word directly
    call .validSep
    jne .badDate
    call getByte
    jc .badDate
    mov byte [td3], al  ;Store the day
    call .validSep
    jne .badDate
    call getByte
    jc .badDate
    mov byte [td4], al  ;Store month
    jmp short .writeHop
.doYear:
    cmp eax, 119    ;If this is larger than 119, return assuming valid
    reta
;If the user specifies 80-99 it means 1980-1999
;If the user specifies 00-79 it means 2000-2079
    mov ebx, 2000
    mov ecx, 1900
    cmp eax, 80     
    cmovb ecx, ebx
    add eax, ecx
    return
.validSep:
    lodsb           ;Get the char and return
    cmp al, "."
    rete
    cmp al, "/"
    rete
    cmp al, "-"
    return

time:
    lea rsi, qword [r8 + cmdLine]
    call skipDelimiters
    cmp byte [rsi], CR  ;If nothing, get input
    jne .goTime  ;Else rsi is pointing to something possibly a time. Try it!
.init:
    lea rdx, curTime
    mov ah, 09h
    int 21h
    call printFmtTime
.noCur:
    lea rdx, newTime
    mov ah, 09h
    int 21h

    lea rdx, cLineBuffer  ;Make sure to use cLineBuffer to preserve F3
    mov byte [rdx], inLen ;Enter a string of up to 128 chars in length
    mov ah, 0Ah
    int 21h
    push rdx
    lea rdx, crlf
    mov ah, 09h
    int 21h
    pop rdx
    cmp byte [rdx + 1], 0   ;If the user typed nothing...
    rete    ;Exit!
    lea rsi, qword [rdx + 2]    ;Go to the text portion
.goTime:
    mov dword [td1], 0          ;Set all fields to 0
    xor eax, eax   
    call getByte
    jc .badTime         ;Get the number in eax
    mov byte [td2], al  ;Save hours
    call .validsep
    jne .badTime
    call getByte
    jc .badTime
    mov byte [td1], al  ;Save minutes
    call .validsep
    je .goSec
    dec rsi ;Go back a char
    call skipDelimiters ;Skip any delimiters
    cmp byte [rsi], CR
    je .setTime
    jmp short .badTime
.goSec:
    call .checkNum
    jc .badTime
    call getByte
    jc .badTime
    mov byte [td4], al  ;Save seconds
    lodsb       ;Move rsi forwards
    cmp al, "." ;Now we dont allow for colon now, only dot!
    je .goMsec
    dec rsi ;Go back a char
    call skipDelimiters ;Skip any delimiters
    cmp byte [rsi], CR
    je .setTime
    jmp short .badTime
.goMsec:
    call .checkNum
    jc .badTime
    call getByte
    jc .badTime
    mov byte [td3], al  ;Save miliseconds
.setTime:
    call skipDelimiters
    cmp byte [rsi], CR  ;Must be terminated by a CR!!
    jne .badTime
    movzx ecx, word [td1]   ;Get hour/minutes
    movzx edx, word [td3]   ;Get seconds/miliseconds
    mov eax, 2D00h      ;Set time
    int 21h
    test al, -1
    retz
.badTime:
    lea rdx, badTime
    call printString
    jmp time.noCur
.validsep:
    lodsb
    cmp al, byte [ctryData + countryStruc.timeSep]  ;Usually a colon
    rete
    cmp al, "."
    return
.checkNum:
    lodsb   ;Now ensure the first char past the delim is a number
    dec rsi
    cmp al, "0"
    jb .cnbad
    cmp al, "9"
    ja .cnbad
    clc
    return
.cnbad:
    stc
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
.loadSwap:
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
    mov eax, 4600h
    int 21h
    inc ecx         ;STDOUT
    mov eax, 4600h
    int 21h
    inc ecx         ;STDERR
    mov eax, 4600h
    int 21h
    mov eax, 3E00h ;Now we close the original handle
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
    mov ecx, 1024   ;Check in a 1024 byte window for a Int 30h call
.biosCheck:
    cmp word [rbx], 30CDh
    je .biosConfirmed
    cmp word [rbx], 0CF48h   ;CFh = IRET, 48h=REX.W
    je .doAnsi      ;If we hit an IRETQ, assume not BIOS
    inc rbx         ;Else, go to next byte for checking
    dec ecx
    jnz .biosCheck
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
.biosConfirmed:
    ;Supports a SCP/BIOS compatible routine, use BIOS   
    mov ah, 0Bh  ; Set overscan to black (when Graphics becomes supported)
    xor ebx, ebx
    int 30h
    mov ah, 0Fh ;Get screen mode
    int 30h
    push rbx
    movzx edx, ah   ;Get number of columns in dl
    dec dl
    mov dh, 25  ;Number of rows is standard
    xor eax, eax
    mov ecx, eax
    mov bh, 7   ;Screen attributes
    mov ah, 6   ;Scroll
    int 30h
    xor edx, edx    ;Set cursor coordinates to top left of screen
    pop rbx
    mov ah, 2
    int 30h
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
    lea rsi, qword [r8 + cmdLine]
    add rsi, rax    ;Point to the path componant
    lea rdi, searchSpec
    push rdi
    call cpDelimPathToBufz  ;Copy the path to rdi
    pop rdi
    cmp byte [rdi + 1], ":"  ;Is this a drive sep?
    je badArgError  ;Cannot pass a path as the second argument
;Check if a drive specifier has been given
    movzx eax, byte [arg1Off]
    lea rsi, qword [r8 + cmdLine]
    add rsi, rax    ;Point to the path componant
    lodsw   ;Get the first two chars
    cmp ah, ":" ;If not, current drive
    jne .curDrvSrc
;Else, copy over the drive letter from the source path, if one exists!
    mov rsi, qword [srcPtr]
    mov word [rsi], ax
    add qword [srcPtr], 2   
    mov rsi, qword [destPtr]
    mov word [rsi], ax
    add qword [destPtr], 2  ;Advance the pointer, to copy to after this ptr
.curDrvSrc:
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
    cmp al, -1  ;Is the source drive bad?
    je badArgError
    lea rsi, searchSpec ;Are we are the head of the buffer?
    ;Now we check if we have a path to actually handle
    cmp rbx, rsi
    je .noPath
    mov byte [rbx - 1], 0   ;The previous char is a pathsep, overwrite it!
    lea rsi, searchSpec
    mov rdi, qword [srcSpec]
    push rsi
    call strcpy ;Copy the string, leave the ptrs past the null
    pop rsi
    dec rdi     ;Point rdi to the null
    mov al, byte [pathSep]
    stosb       ;Store the pathsep and advance the pointer
    mov qword [srcPtr], rdi ;Store this ptr here
    mov rdi, qword [destPtr]    ;Get the ptr (if it was advanced)
    call strcpy ;Copy the string, leave the ptrs past the null
    dec rdi     ;Point rdi to the null
    stosb       ;Store the pathsep and adv the ptr
    mov qword [destPtr], rdi
    mov byte [rbx - 1], al  ;Now replace the null with a pathsep again!
.noPath:
    ;Now we have where to copy the files to, we can start our work!
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
    call .ren   ;Fail silently on wildcard rename
    mov eax, 4F00h  ;Find next
    int 21h
    jnc .wcLoop     ;And process it too!
    return          ;Else just return!
.noWC:
    call buildCommandPath   ;Copy the source arg into searchspec
    movzx eax, byte [arg2Off]
    lea rsi, qword [r8 + cmdLine]
    add rsi, rax    ;Go to the start of the command to source chars
    mov rdi, qword [destPtr]    
    call cpDelimPathToBufz
    lea rdx, searchSpec
    call .ren
    retnc   ;Return if all oki!
    jmp badDupFnf   ;Always just return this
.ren:
    lea rdi, destSpec
    mov eax, 5600h
    int 21h
    return

truename:
    test byte [arg1Flg], -1
    jz badArgError
    call buildCommandPath   ;Get the first argument into the searchSpec
    mov eax, dword [searchSpec]
    shr eax, 8  ;Drop the first char 
    cmp al, ":" ;If its not a drive letter, go search
    jne .goSearch
    shr eax, 8  ;Drop the colon too
    test al, al
    jz .kludge
    cmp al, byte [pathSep]  ;Was path at least X:\ ?
    jne .goSearch
    shr eax, 8  ;Was the path exactly X:\<NUL>?
    jnz .goSearch
.kludge:
    movzx eax, byte [pathSep]
    mov ah, "." ;Add the dot to the second byte
    mov dword [searchSpec + 2], eax ;Add the pathsep, CD dot and null terminator here too
.goSearch:
    lea rsi, searchSpec     ;Store the path here
    mov rdi, rsi    ;Normalise the pathspec here
    mov eax, 6000h  ;TRUENAME
    int 21h
    jnc .writePath
    cmp al, errFnf
    je badFileError
    jmp badParamError
.writePath:
    call printCRLF
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
    mov eax, 5200h  ;Get Sysvars :)
    int 21h
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
    mov ecx, 15 ;Makes space for up to (999 Gb of memory)
    call printDecimalValLB
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

