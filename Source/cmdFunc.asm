;Note! Each function must setup the DTA to use for itself.
;There is no requirement to preserve the DTA across a call.
;Only the Int 2Eh entry point will preserve the callers DTA.

;Common Error Messages, jumped to to return from
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
badDirError:
    lea rdx, badDir
badCmn:
    mov eax, 0900h
    int 21h
    stc ;Return with CY => Error occured
    return

dir:
    ;breakpoint
    mov byte [dirPrnType], 0    ;Clear DIR flags
    mov byte [dirLineCtr], 0
    mov byte [dirFileCtr], 0
    mov byte [dirPathArg], 0    ;Null terminate the start of the buffer
    mov rax, "????????"
    lea rdi, dirSrchPat ;Start also by initialising the search pattern
    stosq
    inc rdi ;Go past dot
    mov word [rdi], ax
    mov byte [rdi + 2], al
    ;Start by scanning for the switches
    lea rdi, cmdBuffer + 1  ;Goto command line input chars count
    movzx ecx, byte [rdi]   ;Get number of chars typed
    inc rdi ;Goto first char typed in
    mov rsi, rdi    ;Use rsi as start of buffer counter
    mov al, byte [switchChar]   ;Scan for switchchars
.switchScan:
    repne scasb ;Scan for a switchchar
    jecxz .switchScanDone
    and al, 0DFh    ;UC it
    cmp al, "W" ;Wide print mode?
    jne .notWideSw
    or byte [dirPrnType], 1 ;Set the correct bit
    jmp short .switchScan
.notWideSw:
    cmp al, "P" ;Pause mode?
    jne badParamError   ;If a switch other than /P or /W, fail
    or byte [dirPrnType], 2 ;Set correct bit
    jmp short .switchScan
.switchScanDone:
;If no args, only switches, we search CWD
;If one arg, search that 
;If more than one, fail
    lea rsi, cmdBuffer + 2
    call skipSpaces ;Skip leading spaces
    add rsi, 3  ;Go past the DIR (always three chars)
.lp:
    call skipSpaces ;Skip spaces after
    lodsb   ;Get first non space char
    call isALEndOfCommand   ;If this is the end char CR or "|", exit
    jz .eocNoNull
    cmp al, ">"
    je .eocNoNull
    cmp al, "<"
    je .eocNoNull
    cmp al, byte [switchChar]  ;Is al a switch char?
    jne .notSwitch
    ;Now we skip the switch if it was a switch
    call findTerminatorOrEOC    ;Go past the switch
    jc .eocNoNull  ;If we reach the EOC, exit,
    jmp short .lp
.notSwitch:
    ;If not a switch, should be a path. Copy to buffer and keep searching
    cmp byte [dirPathArg], 0    ;If a second path provided, error
    jne badArgError
    lea rdi, dirPathArg ;Store the path to search here AS WRITTEN BY USER
    dec rsi ;Go back to the start of the string
.nameCopy:
    lodsb
    call isALEndOfCommand
    jz .eocReached
    call isALterminator
    jz .terminateCopy
    stosb
    jmp short .nameCopy
.terminateCopy:
    xor eax, eax
    stosb   ;Store a terminating null here if a terminator found.
    jmp short .lp ;Now search if another 
.eocReached:
    xor eax, eax
    stosb   ;Store a terminating null here if a terminator found.
.eocNoNull:
    cmp byte [dirPathArg], 0    ;If no path provided, use CWD for current drive
    je .currentDrv
    ;Here we check if we have a drvSpec and path or just drvSpec
    lea rsi, dirPathArg
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
    dec al  ;Convert to 0 based drive number
    mov byte [dirDrv], al
    jmp short .dirPrintVol
.currentDrv:
    call getCurrentDrive    ;Get current drive number (0 based) in al
    mov byte [dirDrv], al   ;Store the 0 based drive number in al
.dirPrintVol:
    movzx eax, byte [dirDrv] 
    call volume.dirEP
    cmp byte [dirPathArg], 0    ;Null path here, 
    je .printCWD
    cmp byte [dirPathArg + 3], 0    ;Was this X:,0?
    je .printCWD
    ;Here we have a path
    ;Temp measure, we just fall through ignoring the path provided
.printCWD:
    mov dl, byte [dirDrv]
    mov al, dl
    add al, "A"
    mov ah, ":"
    mov word [searchSpec], ax
    mov al, byte [pathSep]
    mov byte [searchSpec + 2], al
    lea rsi, searchSpec + 3  ;Make space for a X:"\"
    mov ah, 47h ;Get Current Working Directory
    inc dl  ;Convert to 1 based number
    int 21h
    lea rdi, searchSpec
    call strlen
    dec ecx
    mov byte [rdi + rcx], "$"   ;Replace the null with a string terminator
    lea rdx, dirMain
    mov ah, 09h
    int 21h
    mov rdx, rdi    ;Print the current directory we are working on
    mov ah, 09h
    int 21h
    lea rdx, crlf2
    mov ah, 09h
    int 21h
    call .searchForFile
    return
    ;If we get no path spec or just a X: path spec then we 
    ; search the current working directory for that pathspec
    ;If we get an actual pathspec, we first save the CWD for that drive
    ; and then we try to make the pathspec the CWD. 
    ;   If it works, we then search *.* in that folder and return the og CWD.
    ;   If it fails, we then search one level up, for the search pattern
    ;    that we compute.

    
    ;Now we need to print the path to the folder we will be searching in
.searchForFile:
    ;Now we search for the files
    ;RCX must have the number of chars to the end of the pathspec
    lea rdi, searchSpec
    mov al, byte [pathSep]
    cmp byte [rdi + rcx - 1], al
    je .noAddSlash  ;Deals with special case of root dir
    mov byte [rdi + rcx], al
    inc ecx
.noAddSlash:
    lea rdi, qword [rdi + rcx]
    lea rsi, dirSrchPat
    mov rdx, rdi    ;Ptr to search for in rdx
    movsq
    movsd
    xor al, al
    stosb   ;Store the terminating null
    call setDTA
    lea r10, cmdFFBlock
    mov ecx, dirReadOnly | dirDirectory
    mov ah, 4Eh ;Find first
    int 21h
    jc .dirNoMoreFiles
.findNext:
    call .dirPrintFileData  ;Print the file information
    mov ah, 4Fh
    int 21h
    jnc .findNext 
.dirNoMoreFiles:
    test byte [dirPrnType], 1
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
    test byte [dirPrnType], 1
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
    cmp byte [r8 + fcb1 + fcb.filename], " "
    jne .setPath
    ;If we searched for a . or .., the fcb will be blank. Make sure we didn't search that
    movzx eax, byte [arg1Off]
    lea rsi, cmdBuffer
    add rsi, rax
    mov al, byte [pathSep]
    cmp byte [rsi], al  ;Is the first char a pathsep?
    je .setPath
    cmp byte [rsi], "."
    jne .printDiskCWD
.setPath:
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
    lea rsi, cmdBuffer
    movzx eax, byte [arg1Off]
    add rsi, rax    ;Go to the start of the command
    ;rsi points to terminating char
    lodsb   ;Get first char in AL
    dec rsi ;Go back to this char
    call isALEndOfCommand
    jc badParamError
    lea rdi, sourcePath ;Store this in sourcePath
.copyName1:
    lodsb
    call isALEndOfCommand
    je badParamError
    call isALterminator
    jz .endOfName1
    stosb
    jmp short .copyName1
.endOfName1:
    xor eax, eax
    stosb   ;Store this 0 at rdi
    lea rsi, cmdBuffer
    movzx eax, byte [arg2Off]
    add rsi, rax    ;Go to the start of the command
    lodsb   ;Get first char in AL
    dec rsi ;Go back to this char
    call isALEndOfCommand
    jc badParamError
    lea rdi, destPath
.copyName2:
    lodsb
    call isALEndOfCommand
    je .endOfName2
    call isALterminator
    jz .endOfName2
    stosb
    jmp short .copyName2
.endOfName2:
    xor eax, eax
    stosb   ;Store this 0 at rdi
;Before we open, we check if the two filenames are equal
; If so, crap out.
    lea rsi, sourcePath
    lea rdi, destPath
    mov eax, 121Eh
    int 2Fh
    jz .sameFilename
    ;Open source with read permission
    ;Open destination with write permission
    lea rdx, sourcePath
    mov eax, 3D00h  ;Read open
    int 21h
    jc badParamError
    mov word [sourceHdl], ax

    movzx ebx, ax   ;For bx
    mov eax, 4400h  ;Get device info in dx
    int 21h
    mov word [srcHdlInfo], dx   ;Store information here

    lea rdx, destPath
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
    call buildCommandPath
    lea rdx, searchSpec
    mov eax, 4100h  ;Delete File 
    xor ecx, ecx
    int 21h
    jc badArgError
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
    lea rsi, cmdBuffer
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
;If an ANSI driver is not installed, this will simply insert blank lines
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
    lea rsi, cmdBuffer
    movzx eax, byte [arg1Off]
    add rsi, rax    ;Go to the start of the command
    ;rsi points to terminating char
    lodsb   ;Get first char in AL
    dec rsi ;Go back to this char
    call isALEndOfCommand
    jc badParamError
    lea rdi, sourcePath ;Store this in sourcePath
.copyName1:
    lodsb
    call isALEndOfCommand
    je badParamError
    call isALterminator
    jz .endOfName1
    stosb
    jmp short .copyName1
.endOfName1:
    xor eax, eax
    stosb   ;Store this 0 at rdi
    lea rsi, cmdBuffer
    movzx eax, byte [arg2Off]
    add rsi, rax    ;Go to the start of the command
    cmp byte [rsi + 1], ":" ;If dest path char 2 is :, must be X:, not allowed
    je badParamError
    lodsb   ;Get first char in AL
    dec rsi ;Go back to this char
    call isALEndOfCommand
    jc badParamError
    lea rdi, destPath
.copyName2:
    lodsb
    call isALEndOfCommand
    je .endOfName2
    call isALterminator
    jz .endOfName2
    stosb
    jmp short .copyName2
.endOfName2:
    xor eax, eax
    stosb   ;Store this 0 at rdi
    lea rdx, sourcePath
    lea rdi, destPath
    mov eax, 5600h
    int 21h
    retnc   ;Return if all oki!
    cmp al, errBadDrv
    je badDriveError
    cmp al, errBadFmt
    je badDirError
    jmp badDupFnf
;TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP
touch:
;Temporarily used to create files
    test byte [arg1Flg], -1
    jz badArgError
    call buildCommandPath
    lea rdx, searchSpec
    mov eax, 3C00h  ;Create file 
    xor ecx, ecx
    int 21h
    jc .touchError
    movzx ebx, ax
    mov eax, 3e00h  ;Close file immediately
    int 21h
    return
.touchError:
    lea rdx, touchErr
    jmp badCmn

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
    ;Explicitly call Truename if we remove truename from this function
    movzx eax, byte [arg1Off]
    lea rdx, cmdBuffer
    add rdx, rax    ;Go to the start of the pathname
    mov ecx, 120    ;Only search within 128 chars
    mov al, CR     ;Search for the first space char after the argument
    mov rdi, rdx
    repne scasb
    dec rdi ;Go back a char
    mov byte [rdi], 0   ;Make ASCIIZ
    mov rbp, rdi    ;Have rbp point to the end of the string
    mov rsi, rdx    ;Point rsi to start of path
    lea rdi, searchSpec ;Store the path here
    mov eax, 6000h  ;TRUENAME
    int 21h
    jnc .writePath
    cmp al, 2
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
    lea rsi, cmdBuffer + 2  ;Get the command buffer
    call skipSpaces
    add rsi, 3  ;Go past the VOL command
    call skipSpaces
    lodsb   ;Get the first char, and point rsi to next char
    call isALEndOfCommand   ;If this char is end of command, use current drive
    jnz .checkDriveLetter
    call getCurrentDrive    ;Get 0-based current drive number in al
    jmp short .dirEP
.checkDriveLetter:
    cmp byte [rsi], ":" ;If this is not a :, fail
    jne badDriveError
    mov rdi, rsi    ;Save start of drive spec in rsi
    inc rsi  ;Go past the X: spec
    call skipSpaces
    lodsb   ;Get the non-space char in al
    call isALEndOfCommand   ;The next non-space char must be terminator
    jne badDriveError
;This argument should've been parsed into FCB1 so use that result
    mov al, byte [arg1FCBret]   ;Get the response from the parse
    test al, -1
    jnz badDriveError ;Can't have either wildcards nor be invalid (obviously)
    movzx eax, byte [r8 + fcb1 + fcb.driveNum] ;Get the 1-based drive number
    dec eax ;Convert to 0 based number
.dirEP: ;Must be called with VALID 0 based drive number in al
    add eax, "A" ;Get ASCII representation of 0 based number
    mov byte [volPathBuf], al   ;Store ASCII letter here
    call setDTA
    lea rdx, volPathBuf
    mov cx, dirVolumeID
    mov ah, 4Eh ;Find first
    int 21h
    jc .skipVolLbl
    lea rsi, qword [cmdFFBlock + ffBlock.asciizName]
    lea rdi, volLblSpc
    mov ecx, 11 ;Get the 11 chars of the volume label
.dirLblCopy:
    lodsb   ;Get the first char
    cmp al, 0
    je .skipVolLbl  ;Jump with CF=NC
    cmp al, "."
    je .dirLblSkipStore
    stosb
.dirLblSkipStore:
    dec ecx
    jnz .dirLblCopy
    ;Fallthru with CF=NC
.skipVolLbl:
;Print volume label information now
;Propagates the CF if CF was set    
    pushfq
    lea rdx, crlf
    mov ah, 09h
    int 21h
    lea rdx, volMes
    mov ah, 09h
    int 21h
    mov dl, byte [volPathBuf]   ;Print the drive letter out
    mov ah, 02h
    int 21h
    popfq
    jnc .volIDOk
    lea rdx, volNo
    mov ah, 09h
    int 21h
    lea rdx, crlf
    mov ah, 09h
    int 21h
    return
.volIDOk:
    lea rdx, volOk
    mov ah, 09h
    int 21h
    lea rdi, volLblSpc
    call strlen
    dec ecx
    mov byte [rdi + rcx], "$"   ;Replace the null with a string terminator
    lea rdx, volLblSpc
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
    lea rsi, cmdBuffer
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
    call copyArgumentToSearchSpec
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
;We run EXEC on this and the child task will return via applicationReturn
;Here we must search the CWD or all path componants before failing
;Also this command must be a .COM, .EXE or .BAT so check that first
    call setDTA

    mov eax, dword [cmdFcb + fcb.fileext]   ;Get a dword, with dummy byte 3
    and eax, 00FFFFFFh  ;Clear byte three
    or eax,  20000000h  ;Add a space so it is like "COM "
    cmp eax, "    " ;Only if we have four spaces do we proceed here
    je .noExt
    call checkExtensionExec ;ZF=ZE => Executable
    jnz .dfltErrExit
    ;!!!!!!!!!!!TEMPORARY MEASURE TO AVOID LAUNCHING BAT FILES!!!!!!!!!!!
    jc .dfltErrExit ;Remove this when ready to launch batch files
    ;!!!!!!!!!!!TEMPORARY MEASURE TO AVOID LAUNCHING BAT FILES!!!!!!!!!!!
    ;So it is a com or exe that we are searching for for now
    lea rdi, cmdPathSpec
    mov rdx, rdi
    jmp short .search
.noExt:
    ;If the filename has no extension, append a .*
    ;Use bl as flags. bl[0] => COM found, bl[1] => EXE found, bl[2] => BAT found
    xor ebx, ebx
    lea rdi, cmdPathSpec
    mov rdx, rdi
    xor eax, eax
    mov ecx, -1
    repne scasb
    dec rdi ;Point to the terminating null
    mov rbp, rdi    ;Temporarily store the ptr to the . in rbp
    mov ax, ".*"
    stosw
    xor al, al  ;Store terminating null
    stosb
.search:
    mov ecx, dirIncFiles
    mov ah, 4Eh ;Find First File
    int 21h
    jc .dfltErrExit
    call .noExtCheckExt
.moreSearch:
    mov ah, 4Fh
    int 21h
    jc .noMoreFiles
    call .noExtCheckExt
    jmp short .moreSearch
.noMoreFiles:
    test ebx, ebx
    jz .dfltErrExit
;So we have a valid executable
    mov rdi, rbp    ;Get back ptr to the .*,0
    test ebx, 1
    jz .launchexebat
    mov eax, ".COM"
    jmp short .buildTail
.launchexebat:
    test ebx, 2
    jz .launchbat
    mov eax, ".EXE"
    jmp short .buildTail
.launchbat:
;Temporary For BAT
    jmp .dfltErrExit
.buildTail:
    stosd
    xor eax, eax
    stosb   ;Store the terminating null
;Now we build the cmdtail properly
    lea rdi, cmdTail
    mov rdx, rdi    ;Use rdx as the anchor pointer for cmdline
    mov ecx, 128/8
    rep stosq   ;Clear the buffer with nulls
    lea rdi, qword [rdx + 1]    ;Mov rdi to start of cmdtail (not count)
    lea rsi, qword [r8 + cmdLineCnt]
    lodsb   ;Get into al the number of chars and move rsi to the tail proper
    mov ah, al  ;Move the number into ah
    xor ecx, ecx    ;Use ch for number of chars read, cl for chars copied
    ;Skip the parsed command name
.passName:
    lodsb   ;Get the char in al
    inc ch
    cmp ch, 127     ;Exit condition (bad case)
    je short .finishBuildingTailNoCR
    cmp al, CR      ;If we get to the CR after name, no tail
    je short .finishBuildingTail
    cmp al, SPC
    jne short .passName
    ;Now we copy the name 
    call skipSpaces ;Start by skipping spaces (there are no embedded tabs)
    ;rsi points to the first non-space char
.copyTail:
    lodsb
    cmp al, CR
    je short .finishBuildingTail
    stosb
    inc cl
    cmp cl, 127 ;Exit condition
    jne .copyTail
    jmp short .finishBuildingTailNoCR
.finishBuildingTail:
    stosb   ;Store the CR
.finishBuildingTailNoCR:
    mov byte [rdx], cl  ;Finish by placing count 
.launch:
    lea rbx, launchBlock
    mov rax, qword [r8 + psp.envPtr]    ;Get the env pointer
    mov qword [rbx + execProg.pEnv], rax 
    lea rax, cmdTail
    mov qword [rbx + execProg.pCmdLine], rax
    lea rax, qword [r8 + fcb1]
    mov qword [rbx + execProg.pfcb1], rax
    lea rax, qword [r8 + fcb2]
    mov qword [rbx + execProg.pfcb2], rax
    lea rdx, cmdPathSpec
    mov eax, 4B00h  ;Load and execute!
    int 21h
    jmp .dfltErrExit    ;If something goes wrong, error out
.noExtCheckExt:
    ;mov eax, dword [cmdFFBlock + ffBlock.asciizName + filename.fExt]
    lea rsi, dword [cmdFFBlock + ffBlock.asciizName]
    lea rdi, fcbCmdSpec
    call asciiFilenameToFCB
    mov eax, dword [fcbCmdSpec + filename.fExt]
    and eax, 00FFFFFFh  ;Clear byte three
    or eax,  20000000h  ;Add a space so it is like "COM "
    cmp eax, "COM "
    jne .neceexe
    or ebx, 1
    return
.neceexe:
    cmp eax, "EXE "
    jne .necebat
    or ebx, 2
    return
.necebat:
    cmp eax, "BAT "
    retne
    or ebx, 4
    return

.dfltErrExit:
    lea rdx, badCmd
    mov ah, 09h
    int 21h
    return

.cmdTailTerminatorCheck:
;Input: al = Char to check
;ZF=NZ -> Not a terminator
;ZF=ZE and CF=NC -> SPC detected
;ZF=ZE and CF=CY -> CR detected
    clc     ;Clear CF 
    cmp al, SPC
    rete    ;ZF Set
    cmp al, CR
    retne   ;CF and ZF clear
    stc     ;Set CF since ZF is already set
    return