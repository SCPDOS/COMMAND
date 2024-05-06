;Misc functions and subroutines for command.com

printDate:
;Input: eax = Packed Date
;       eax[0:4] = Day of the month, a value in [0,...,31]
;       eax[5:8] = Month of the year, a value in [0,...,12]
;       eax[9:15] = Number of years since 1980, a value in [0,...,127]
;       ebx = 0 => Print two digit year
;       ebx = 1 => Print four digit year
    push rbx
    mov ecx, eax    ;Save in ecx temporarily
    cmp byte [ctryData + countryStruc.dtfmt], 1
    jb .usDate
    ja .jpnDate
;European: DD/MM/YY
    and eax, 1Fh    ;Save day bits
    call .printFirst
    mov dl, byte [ctryData + countryStruc.dateSep]
    mov ah, 02h
    int 21h

    mov eax, ecx
    and eax, 1E0h   ;Save bits 5-8
    shr eax, 5
    call .printSecond

    mov dl, byte [ctryData + countryStruc.dateSep]
    mov ah, 02h
    int 21h

    mov eax, ecx
    and eax, 0FE00h ;Save bits 9-15
    shr eax, 9
    pop rbx ;Get the year signature back
    call .printYear
    return
.usDate:
;US: MM/DD/YY
    and eax, 1E0h   ;Save bits 5-8
    shr eax, 5
    call .printFirst

    mov dl, byte [ctryData + countryStruc.dateSep]
    mov ah, 02h
    int 21h

    mov eax, ecx
    and eax, 1Fh    ;Save day bits
    call .printSecond

    mov dl, byte [ctryData + countryStruc.dateSep]
    mov ah, 02h
    int 21h

    mov eax, ecx
    and eax, 0FE00h ;Save bits 9-15
    shr eax, 9
    pop rbx ;Get the year signature back
    call .printYear
    return
.jpnDate:
;Japan: YY/MM/DD
    and eax, 0FE00h ;Save bits 9-15
    shr eax, 9
    pop rbx ;Get the year signature back
    call .printYear

    mov dl, byte [ctryData + countryStruc.dateSep]
    mov ah, 02h
    int 21h

    mov eax, ecx
    and eax, 1E0h   ;Save bits 5-8
    shr eax, 5
    call .printSecond

    mov dl, byte [ctryData + countryStruc.dateSep]
    mov ah, 02h
    int 21h

    mov eax, ecx
    and eax, 1Fh    ;Save day bits
    call .printSecond
    return

;Each of these require eax setup correctly
.printFirst:
    push rcx
    call getDecimalWord
    test ch, ch ;Do we have an upper digit?
    jnz .skipSpace
    mov ch, cl
    mov cl, " "
.skipSpace:
    mov dl, cl
    mov ah, 02h
    int 21h
    mov dl, ch
    mov ah, 02h
    int 21h
    pop rcx
    return
.printSecond:
    push rcx
    call getDecimalWord
    test ch, ch ;Do we have an upper digit?
    jnz .skipSpace
    mov ch, cl
    mov cl, "0"
    jmp short .skipSpace
.printYear:
    add eax, 1980
    push rcx
    push rbx
    call getDecimalWord ;Get unpacked in rcx
    pop rbx
    test bl, bl
    jz .twoDigitYear
    mov dl, cl  ;Print the first digit
    mov ah, 02h
    int 21h
    mov dl, ch  ;Print the second digit
    mov ah, 02h
    int 21h
.twoDigitYear:
    shr ecx, 10h    ;Get high word low
    mov dl, cl  ;Print the upper digit
    mov ah, 02h
    int 21h
    mov dl, ch  ;Print the lower digit
    mov ah, 02h
    int 21h
    pop rcx
    return


printTime:
;Input: eax = Packed Time
;       eax[5:10] = Minutes, a value in [0,...,59] 
;       eax[11:15] = Hours, a value in [0,...,23]
    mov ecx, eax
    and eax, 0F800h ;Save bits 11-15
    shr eax, 11
    cmp byte [ctryData + countryStruc.timefmt], 1  
    jne .ampm
    call .printHours
    call .printMinutes
    return
.ampm:
    cmp eax, 12
    ja .pm
    je .pm2
    call .printHours
    call .printMinutes
    mov dl, "a"
    mov ah, 02h
    int 21h
    return
.pm:
    sub eax, 12
.pm2:
    call .printHours
    call .printMinutes
    mov dl, "p"
    mov ah, 02h
    int 21h
    return
.printMinutes:
    mov dl, byte [ctryData + countryStruc.timeSep]
    mov ah, 02h
    int 21h

    mov eax, ecx
    and eax, 7E0h   ;Save bits 5-10
    shr eax, 5
.printMinutesAlt:
    push rcx
    call getDecimalWord
    test ch, ch ;Do we have an upper digit?
    jnz .skipSpace
    mov ch, cl
    mov cl, "0"
    jmp short .skipSpace
.printHours:
    push rcx
    call getDecimalWord
    test ch, ch ;Do we have an upper digit?
    jnz .skipSpace
    mov ch, cl
    mov cl, " "
.skipSpace:
    mov dl, cl
    mov ah, 02h
    int 21h
    mov dl, ch
    mov ah, 02h
    int 21h
    pop rcx
    return

printPrompt:
    test byte [echoFlg], -1 ;If echo off, no prompt!
    retz
    lea rdi, promptEVar   ;Find the prompt var
    call searchForEnvVar
    jnc .validPrompt
    ;Here we print the default prompt
    call putCWDInPrompt
    call putGTinPrompt
    return
.validPrompt:
;rsi points to the prompt
    add rsi, 7  ;Go past the prompt= string
.promptLp:
    lodsb   ;Get the char
    test al, al
    retz    ;Return when al is 0
    cmp al, "$"
    je .escapeChar
    mov dl, al
    call outChar    ;Prints the char in dl
    jmp short .promptLp
.escapeChar:
    lodsb       ;Get the next char now that we are escaping
    call ucChar ;Uppercase this char in al
    lea rbx, .pTbl
    mov ecx, pTblL / 3  ;3 bytes per entry
.escapeLp:
    cmp byte [rbx], al  ;Are we at the right entry?
    je .escapeFnd
    add rbx, 3  ;Go to next entry
    dec ecx
    jnz .escapeLp
    jmp short .promptLp  ;Output nothing if invalid
.escapeFnd:
    lea rax, .pTbl              ;Get the table addr
    movzx ebx, word [rbx + 1]   ;Get the word offset from the table
    add rax, rbx                ;Add these
    push rsi    ;Ensure rsi remains unchanged
    call rax    ;Call indirectly the function to output the chars
    pop rsi
    jmp short .promptLp

;Easy table to use, 13 entries, 3 bytes per entry
.pTbl:
    db "B", 
    dw putPipeInPrompt - .pTbl     ;Pipe char
    db "D", 
    dw printFmtDate - .pTbl        ;Current date
    db "E", 
    dw putEscInPrompt - .pTbl      ;ANSI Escape char
    db "G", 
    dw putGTinPrompt - .pTbl       ;Greater than char
    db "H", 
    dw putBSPinPrompt - .pTbl      ;Backspace
    db "L", 
    dw putLTinPrompt - .pTbl       ;Less than char
    db "N", 
    dw putDriveInPrompt - .pTbl    ;Current drive letter
    db "P", 
    dw putCWDInPrompt - .pTbl      ;Current drive and path
    db "Q", 
    dw putEquInPrompt - .pTbl      ;Equals char
    db "T", 
    dw printFmtTime - .pTbl        ;Current time in hh:mm:ss.hh fmt
    db "V", 
    dw putVersionInPrompt - .pTbl  ;DOS version number
    db "_", 
    dw printCRLF - .pTbl           ;CRLF pair
    db "$", 
    dw putMoneyInPrompt - .pTbl    ;Dollar sign
pTblL equ $ - .pTbl

printCRLFecho:
    test byte [echoFlg], -1
    retz    ;If Echo off, don't do a CRLF!
printCRLF:
    lea rdx, crlf
printString:
    mov eax, 0900h  ;Print string
    int 21h
    return

putVersionInPrompt:
    lea rdx, dosVer
    mov ah, 09h ;Print String
    int 21h
    mov ah, 30h ;Get ver in al=Maj ver, ah = Min ver
    int 21h
    push rax    ;Save minor version
    call hexToBCD   ;Get in al a bcd representation for major version
    call printPackedBCD ;Print al
    mov dl, "."
    mov ah, 02h
    int 21h
    pop rax
    mov al, ah  ;Get the minor version low
    call hexToBCD
    call printPackedBCD
    return
putEscInPrompt:
    mov dl, ESC
    jmp short outChar

putMoneyInPrompt:
    mov dl, "$"
    jmp short outChar

putEquInPrompt:
    mov dl, "="
    jmp short outChar

putPipeInPrompt:
    mov dl, "|"
    jmp short outChar

putGTinPrompt:
    mov dl, ">"
    jmp short outChar

putLTinPrompt:
    mov dl, "<"
    jmp short outChar

putBSPinPrompt:
    lea rdx, backSpace
    mov ecx, 3
    mov ebx, 1
    mov eax, 4000h  ;Output the backspace!
    int 21h
    return

putDriveInPrompt:
    call getCurrentDrive
    add al, "A" ;Convert to letter
    mov dl, al
outChar:
    mov ah, 02h ;Echo to STDOUT
    int 21h
    return
putCWDInPrompt:
    call getCurrentDrive
    cmp byte [failDrv], al  ;If these are equal, badDrive
    mov byte [failDrv], -1  ;Reset on read
    je .badDrive 
    mov dl, al  ;Get drive letter in dl for path
    inc dl
    add al, "A" ;Convert to letter
    mov ah, ":"
    lea rdi, currDirStr ;Update the current directory string
    stosw   ;Store X:, rdi+=2
    mov al, byte [pathSep]
    stosb   ;Store pathSep, inc rdi
    mov eax, 4700h  ;Get Current Working Directory
    mov rsi, rdi    ;rsi points to buffer to write to
    int 21h
    jc .badDrive
    call strlen
    add ecx, 2 ;Add two for the X:
    ;We repurpose the fact that strlen counts the NULL to account for "\"
    mov eax, 4000h ;Write to handle
    mov ebx, 1  ;STDOUT
    lea rdx, currDirStr
    int 21h
    return
.badDrive:
;If the drive is bad, we print this string instead of drive:\cwd
    lea rdx, badDrvMsg
    jmp printString
printFmtTime:
;Outputs the formatted time
    call getTime
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
    return
getTime:
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
    return

printFmtDate:
;Outputs the formatted date
    mov eax, 2A00h ;DOS get date
    int 21h
	;AL = day of the week (0=Sunday)
	;CX = year (1980-2099)
	;DH = month (1-12)
	;DL = day (1-31)
    sub cx, 1980    ;Subtract 1980 from cx
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
    call outChar
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
    return


hexToBCD:
;Converts a Hex byte into two BCD digits
;Takes input in al, returns in al (zero-ed upper seven bytes)
    push rcx
    movzx eax, al   ;Zero extend
    xor ecx, ecx
.htb0:
    cmp eax, 10
    jb .htb1
    sub eax, 10
    inc ecx
    jmp short .htb0
.htb1:
    shl ecx, 4  ;Move to upper nybble
    or al, cl   ;Move upper nybble into al upper nybble
    pop rcx
    ret

printPackedBCD:
;Gets a packed BCD digit in al and prints al[7:4] if non zero,
; then prints al[3:0]. Prints a space if the upper nybble is zero
;Preserves all registers
    push rax
    push rdx
    mov ah, al
    and al, 0Fh     ;Isolate lower nybble
    and ah, 0F0h    ;Isolate upper nybble
    jnz .upperNybble
    mov dl, " "
    mov ah, 02h
    int 21h
    jmp short .lowerNybble
.upperNybble:
    push rax
    shr ah, 4
    add ah, "0"  ;Convert to an ASCII digit
    mov dl, ah
    mov ah, 02h ;Print DL
    int 21h
    pop rax
.lowerNybble:
    add al, "0"
    mov dl, al
    mov ah, 02h ;Print DL
    int 21h
    pop rdx
    pop rax
    return

setDrive:
;Input: dl = 0 based Drive number to set to
;Output: ZF=ZE: Drive set. ZF=NZ: Drive not set and invalid.
;AX trashed.
    mov ah, 0Eh ;Set drive to dl
    int 21h 
    call getCurrentDrive
    cmp al, dl  ;Is this the same drive?
    return
getCurrentDrive:
;Returns the 0 based current drive in al
    mov ah, 19h
    int 21h
    return

strcpy:
;Copies an ASCIIZ string but leaves the pointers at the end of the strings
;rsi -> Source
;rdi -> Destination
    push rcx
    push rdi
    mov rdi, rsi
    call strlen ;Get the length of the string in rsi
    pop rdi
    rep movsb   ;Now we have the count, just copy over!
    pop rcx
    return

strcpy2:
;Copies an ASCIIZ string, but preserves the ptrs
;rsi -> Source
;rdi -> Destination
    push rsi
    push rdi
    call strcpy 
    pop rdi
    pop rsi
    return

strlen:
;Gets the length of a ASCIIZ string
;Input: rdi = Source buffer
;Output: ecx = Length of string, INCLUDING TERMINATING NULL
    push rax
    mov eax, 1212h  ;Strlen according to DOS, trashes eax
    int 2fh
    pop rax
    return

ucChar:
;Input: al = Char to uppercase
;Output: al = Adjusted char 
    push rbx
    push rax    ;Push the char twice on the stack
    mov eax, 1213h  ;Get DOS to uppercase the char
    int 2fh         ;Returns the processed char in al
    pop rbx         ;Pop the old word off the stack
    pop rbx
    return

normalisePath:
;Uppercases chars and flips pathseps
;Input: rsi -> Path to normalise
    push rax
    push rdi
    mov rdi, rsi
    mov eax, 1211h
    int 2fh
    pop rdi
    pop rax
    return

skipDelimiters:
;Skips all "standard" command delimiters. This is not the same as FCB 
; command delimiters but a subset thereof. 
;These are the same across all codepages.
;Input: rsi must point to the start of the data string
;Output: rsi points to the first non-delimiter char
    push rax
.l1:
    lodsb
    call isALdelimiter
    jz .l1
.exit:
    pop rax
    dec rsi ;Point rsi back to the char which is not a command delimiter
    return

isALdelimiter:
;Returns: ZF=NZ if al is not a command separator 
;         ZF=ZE if al is a command separator
    cmp al, " "
    rete
    cmp al, ";"
    rete
    cmp al, "="
    rete
    cmp al, ","
    rete
    cmp al, TAB
    return

clearCommandState:
;Clears the command state
    lea rdi, cmdStatePtr
    mov ecx, cmdStateL
    xor eax, eax
    rep stosb
    return

clearCommandLineState:
;Clears the command line state after a 0Dh encountered
    lea rdi, cmdLineStatePtr
    mov ecx, cmdLineStateL
    xor eax, eax
    rep stosb
    mov dword [pipeSTDIN], -1
    mov dword [redirSTDIN], -1
;Close all handles from 5->MAX
    movzx ecx, word [numHdls]
    mov ebx, 5
.lp:
    mov ah, 3Eh ;File close
    int 21h
    inc ebx ;Goto next file
    cmp ebx, ecx
    jbe .lp    ;Keep looping whilst below or equal
    return

asciiFilenameToFCB:
;Converts a filename in the form FILENAME.EXT,0 to FILENAMEEXT
;Don't uppercase any lowercase chars as this could be used with user buffers.
;Also doesn't check if chars are valid
;Names such as SYS.COM get converted to "SYS     COM"
;Name is space padded.
;Input: rsi = ASCII string buffer
;       rdi = FCB name buffer
;Output: al = Char that terminated the source string 
    push rbx    
    push rdi
    mov ecx, 11
    mov al, " "
    rep stosb   ;Fill the buffer with spaces (so we don't need to fill later)
    pop rdi
    mov rbx, rdi    ;Use rbx as the base pointer of this buffer
    ;Preprocess for Dir Searches
    cmp byte [rsi], "."
    jne .processName
    movsb   ;Store the first dot
    cmp byte [rsi], "."
    jne .exitBadChar
    movsb
    jmp short .exitBadChar
.processName:
    lodsb   ;Get the char in al
    test al, al ;If the char is a null, must be at the end of the name
    jz .exit
    cmp al, " " ;If space or a period, go to extension field. If null, exit
    je .extSpace
    cmp al, "."
    je .ext
    stosb   ;Store the char
    jmp short .processName
.extSpace:
;Now we scan for a period in the name
    lodsb   ;Get a char and increase rsi
    test al, al
    jz .exit
    cmp al, "."     ;If al is not a period...
    jne .extSpace   ; keep searching
.ext:
    lea rdi, qword [rbx + filename.fExt]    ;Put destination at the extension
.processExt:
    lodsb
    test al, al
    jz .exit
    cmp al, " "
    je .exit
    stosb
    jmp short .processExt
.exitBadChar:
    xor al, al  ;Return a null terminator
.exit:
    pop rbx
    return

findLastPathComponant:
;Finds the last path componant of an ASCIIZ path string
;Input: rdi -> Head of the path to find last componant on
;Output: rdi -> Start of the last componant
    push rax
    push rcx
    xor ecx, ecx
    dec ecx
    xor eax, eax
    repne scasb ;Scan for the null terminator of the string
    not ecx     ;This gets the count of chars  
    dec rdi     ;Move rdi back to the null!
    mov al, byte [pathSep]
    std
    repne scasb ;Now scan backwards for the pathsep, or we run out of chars!
    cld
    jnz .exit   ;Ran out of chars to scan! Skip the extra inc
    inc rdi     ;Point at pathsep
.exit:
    inc rdi     ;Point at char after pathsep or first char in buffer
    pop rcx
    pop rax
    return

FCBToAsciiz:
;Converts a filename in the form FILENAMEEXT to FILENAME.EXT,0
;Name is space padded too
;Input: rsi = FCB name buffer
;       rdi = ASCIIZ string buffer
    mov ecx, 8
    rep movsb   ;Move the name over
.scanNameSpace:
    cmp byte [rdi - 1], " " ;Is the previous char a space?
    jne .ext
    dec rdi
    inc ecx
    cmp ecx, 8
    jb .scanNameSpace
.ext:
    cmp word [rsi], "  "    ;Are the first two chars a space?
    jne .validExt
    cmp byte [rsi + 2], " " ;Is the final char a space?
    je .exit
.validExt:
    mov al, "." ;We have a valid extension, store a period
    stosb
    mov ecx, 3
    rep movsb   ;Move the three extension chars over
.scanExtSpace:
    cmp byte [rdi - 1], " " ;Is the previous char a space
    jne .exit
    dec rdi
    jmp short .scanExtSpace
.exit:
    xor eax, eax
    stosb   ;Store a null at the end
    return

cpDelimOrCtrlStringToBufz:
;Copy a delimited or control char terminated string to a buffer
;Input: rsi -> Point to start of delimiter or ctrlchar terminated string
;       rdi -> Buffer to store null terminated string in
;Output: rsi -> First char past string end
;       rdi -> One char past null terminator on string buffer
    mov byte [rdi], 0   ;Init by null terminating
.lp:
    lodsb
    cmp al, 20h ;Chars up to 20h are delimiters here. CR included!
    jbe .exit
    call isALdelimiter
    je .exit
    stosb
    jmp short .lp
.exit:
    xor eax, eax
    stosb   ;Store a null terminator
    return


cpDelimPathToBufz:
;Copy a delimited path into buffer and null terminate.
;Input: rsi -> Point to start of delimiter terminated path
;       rdi -> Buffer to store null terminated path in
;Output: rsi -> First char past pathname delimiter
;       rdi -> One char past null terminator on pathname buffer
    push rbx
    mov rbx, rdi    ;Save the head of the path in rbx
    mov byte [rdi], 0   ;Null terminate this path before starting!
.lp:
    lodsb   ;Get the char
    cmp al, CR
    je .gotRedirPath
    call isALdelimiter  ;Is this char a delimiter char?
    jz .gotRedirPath 
    cmp al, byte [switchChar]
    je .gotRedirPath
    stosb   ;Store this char and loop next char
    jmp short .lp
.gotRedirPath:
    push rax    ;Save the char on stack
    xor al, al  ;Get null terminator char
    sub rbx, rdi
    cmp rbx, -1 ;This is because one char has been written!!
    je .notColon
    cmp rbx, -2 ;This is for drive letters, must always have the colon!!
    je .notColon
    cmp byte [rdi - 1], ":" ;Is this a colon?
    jne .notColon
    dec rdi     ;We overwrite the colon. 
.notColon:
    stosb   ;Store the null terminator for the redir path
    pop rax ;Get back the char in al
    pop rbx
    return

buildCommandPath:
;Copies the first argument into a null delimited path in the searchSpec buffer.
    movzx eax, byte [arg1Off]
    mov r8, [pspPtr]
    lea rsi, qword [r8 + cmdLine]
    add rsi, rax    ;Go to the start of the command
copyArgumentToSearchSpec:
;Copies an arbitrary delimited path pointed to by rsi into searchSpec
; and null terminates
    lea rdi, searchSpec
    call cpDelimPathToBufz
    return

scanForWildcards:
;Input: rsi -> Null terminated path to search for wildcards on
;Output: ZF=ZE if WC found. Else, ZF=NZ.
    push rax
    push rsi
.lp:
    lodsb
    cmp al, "?"
    je .exit
    cmp al, "*"
    je .exit
    test al, al
    jnz .lp
    inc al  ;This will clear the ZF
.exit:
    pop rsi
    pop rax
    return

printDecimalWord:
;Takes qword in rax and print it's decimal representation
;Takes the qword in eax and prints its decimal representation
    xor ecx, ecx
    xor ebx, ebx    ;Store upper 8 nybbles here
    test eax, eax
    jnz .notZero
    mov ecx, "0"
    mov ebp, 1  ;Print one digit
    jmp short .dpfb2
.notZero:
    xor ebp, ebp  ;Use bp as #of digits counter
    mov esi, 0Ah  ;Divide by 10
.dpfb0:
    inc ebp
    cmp ebp, 8
    jb .dpfb00
    shl rbx, 8    ;Space for next nybble
    jmp short .dpfb01
.dpfb00:
    shl rcx, 8    ;Space for next nybble
.dpfb01:
    xor edx, edx
    div rsi
    add dl, '0'
    cmp dl, '9'
    jbe .dpfb1
    add dl, 'A'-'0'-10
.dpfb1:
    cmp ebp, 8
    jb .dpfb10
    mov bl, dl ;Add the bottom bits
    jmp short .dpfb11
.dpfb10:
    mov cl, dl    ;Save remainder byte
.dpfb11:
    test rax, rax
    jnz .dpfb0
.dpfb2:
    cmp ebp, 8
    jb .dpfb20
    mov dl, bl
    shr rbx, 8
    jmp short .dpfb21
.dpfb20:
    mov dl, cl    ;Get most sig digit into al
    shr rcx, 8    ;Get next digit down
.dpfb21:
    mov ah, 02h
    int 21h
    dec ebp
    jnz .dpfb2
    return

getDecimalWord:
;Works on MAX A dword in eax
;Gets the decimalised DWORD to print in rcx (at most 8 digits)
    xor ecx, ecx
    xor ebp, ebp  ;Use bp as #of digits counter
    mov ebx, 0Ah  ;Divide by 10
.dpfb0:
    inc ebp
    shl rcx, 8    ;Space for next nybble
    xor edx, edx
    div rbx
    add dl, '0'
    cmp dl, '9'
    jbe .dpfb1
    add dl, 'A'-'0'-10
.dpfb1:
    mov cl, dl    ;Save remainder byte
    test rax, rax
    jnz .dpfb0
    return
getByte:
;Indicates if the number obtained is larger than a byte by setting CF
    call getNum
    cmp eax, 100h
    cmc
    return
getNum:
;Gets a number from a command line.
;Input: rsi -> String to get number from
;Output: rsi -> Char which terminated the accumulation
;        eax = Value of the string. May overflow if we read more than 9 chars...
;If an overflow, CF=CY. eax=Undefined. rsi-> Char 9
    push rcx
    push rbx
    xor ebx, ebx
    xor ecx, ecx    ;Start with a value of zero
    xor eax, eax    
.lp:
    lodsb
    cmp al, "0"
    jb .exit
    cmp al, "9"
    ja .exit
    sub al, "0"                     ;Convert to binary 
    lea ecx, qword [4*ecx + ecx]    ;5*rcx
    shl ecx, 1                      ;2*(5*rcx)
    add ecx, eax
    inc ebx
    cmp ebx, 9
    jne short .lp   ;Get next digit
    stc             ;Yikesaroony
    jmp short .exitBad
.exit:
    clc             ;Clear CF
.exitBad:
    mov eax, ecx
    pop rbx
    pop rcx
    dec rsi         ;dec doesnt affect CF 
    return


freezePC:
    lea rdx, memBad1
    mov ah, 09h
    int 21h
.altEP:
    lea rdx, memBad3
    mov ah, 09h
    int 21h
.lp:
    pause
    hlt
    jmp short .lp

setDTA:
    push rax
    push rdx
    lea rdx, cmdFFBlock     ;Use this as the DTA for this request
    mov eax, 1A00h
    int 21h
    pop rdx
    pop rax
    return

getDTA:
    lea rdx, cmdFFBlock
    return

resetIDTentries:
;Resets the IDT entries if they were changed by dead child process
    lea rdx, critErrorHandler
    mov eax, 2524h
    int 21h
    lea rdx, ctrlCHandler
    mov eax, 2523h
    int 21h
    lea rdx, appRet
    mov eax, 2522h
    int 21h
    return

;-------------------------------
; Environment utility functions
;-------------------------------

cmpEnvVar:
;Checks that we have found the environment variable we are looking for.
;Input: rsi -> Environment var to verify the name of
;       rdi -> Environment var name to compare against
;Output: ZF=ZE: Equal. ZF=NZ: Not equal.
    push rsi
    push rdi
    xchg rsi, rdi       ;Swap Env and user ptrs
.lp:
    lodsb               ;Pick up from user string
    call ucChar         ;Upper case it!
    cmp byte [rdi], al  
    jne .exit
    cmp al, "="         ;Are we at the end?
    je .exit
    inc rdi
    jmp short .lp
.exit:
    pop rdi
    pop rsi
    return

checkEnvGoodAndGet:
;Gets the env ptr and checks that it is double null terminated.
;Output:
;   ZF=ZE: Environment is bad. Is not double null terminated.
;   ZF=NZ: Environment is good. Is double null terminated.
;           rsi -> Environment pointer
    push rax
    push rcx
    push rdi
    push r8
    mov rdi, qword [r8 + psp.envPtr]    ;Get the env ptr!
    test rdi, rdi   ;Null envs are possible. If it happens, just fail!
    jz .badExit
    mov ecx, dword [rdi - mcb_size + mcb.blockSize] ;Get the mcb size in para
    shl ecx, 4          ;Convert to bytes (max number of bytes in the block!)
;Ensure we have a good environment, i.e. one that is double null terminated.
    xor eax, eax
.pathNulScan:
    repne scasb
    test ecx, ecx   ;If we are zero on first null, its an error
    jz .badExit
    cmp byte [rdi], al  ;Is char two null?
    jne .pathNulScan    ;If not, keep searching
    xor eax, eax
    inc eax ;Clear the ZF
    mov rsi, qword [r8 + psp.envPtr]    ;Return the env ptr here
.badExit:
    pop r8
    pop rdi
    pop rcx
    pop rax
    return


findEnvSpace:
;Searches the environment for space, returns a pointer to the start of the 
; free space in the pointer and a count of free space in the environment.
;If count is zero, pointer is not to be used.
;Output: ecx = Count. rsi -> First free byte in Environment alloc region.
    xor ecx, ecx
    xor eax, eax    ;Clear eax too
    push rcx
    call checkEnvGoodAndGet ;Get environment in rsi
    pop rcx
    retz    ;If ZF=ZE, env bad. Count zero.
    lea rbx, qword [rsi - mcb_size] ;Point rbx to the memory arena header
    dec ecx ;Make into a large counter
    mov rdi, rsi    
.endsearch:
    repne scasb ;Scan
    cmp byte [rdi], 0
    jne .endsearch  ;If not double zero, keep searching
    ;Here we are at the end of the environment
    neg ecx
    inc ecx ;Include the terminating null in the count of the env size.
    mov rsi, rdi    ;Save the pointer to the start of free space in rsi
    mov eax, dword [rbx + mcb.blockSize]    ;Get the size of environment
    sub eax, ecx
    mov ecx, eax    ;Get the final count in ecx
    return

searchForEnvVar:
;Gets the environment, and scans it for a string with the var specified.
;Input: rdi -> Var name to look for.
;Returns: CF=NC: rsi -> Env var in env.
    push rdi
    push rcx
    push rdx
    mov rsi, qword [r8 + psp.envPtr]
    mov rdx, rdi        ;Save the search pointer!
.varLp:
    mov rdi, rdx        ;Reset the pointer for searching
    call cmpEnvVar      ;Checks if these two environment vars are equal
    je .varFound
    xor eax, eax        ;Search for a null
    mov rdi, rsi        ;Scan the environment
    mov ecx, -1         ;Just keep searching
    repne scasb         ;Now scan for the terminating null
    cmp byte [rdi], al  ;Now check the second char
    je .varNotFound     ;If second null, no more env to search!
    mov rsi, rdi        ;Now move to rsi the start of the next env var
    jmp short .varLp    ;And scan again!
.varNotFound:
    stc
.varFound:
    pop rcx
    pop rdx
    pop rdi
    return

envFree:
;Frees a variable from the environment, pulls the strings behind it up
; zeros the rest of the environment, and returns a pointer to the first
; free byte of the environment!
;Input: rsi -> Variable to free.
;Output: rdi -> First byte to write new env var in (old second null)
;        ecx = Number of free bytes in env
    mov rdi, rsi
    xor eax, eax
.freeLp:
    cmp byte [rdi], 0
    je .exitLp
    stosb
    jmp short .freeLp
.exitLp:
;rdi points to the terminating null of the var we just deleted
;rsi points to the start of the free space
    xchg rsi, rdi   ;Swap em!
    cmp word [rsi], 0   ;If we are already at the terminating null, dont advance!
    jne .prepPullup
    xor eax, eax
    jmp short .cleanEnv
.prepPullup:
    inc rsi         ;Go past the terminating null!
.pullUp:
    lodsb
    stosb
    test al, al ;Did we pick up a zero
    jne .pullUp ;If not, keep copying
    cmp byte [rsi], 0   ;Is this the famous second byte?
    jne .pullUp
;We are at the end of the copy!
.cleanEnv:
    stosb   ;Store the famous second null
    dec rdi ;without incrementing it!!
    call getFreeSpace
    xor eax, eax
    push rcx
    rep stosb       ;Now zero the remaining space of the env!
    pop rcx
    return

getFreeSpace:
;Output: ecx = Number of free bytes in the environment block
    push rsi
    push rdi
    call getPtrToEndOfEnv   ;Get ptr in rdi to end of alloc 
    mov rsi, qword [r8 + psp.envPtr]
    sub rdi, rsi    ;This gets number of bytes allocated
    call getEnvSize ;Get total block size
    sub ecx, edi    ;Get difference!
    pop rdi
    pop rsi
    return

getEnvSize:
;Gets the number of bytes in the environment allocation
;Output: ecx = Number of bytes in the environment!
    push rbx
    mov rbx, qword [r8 + psp.envPtr]
    mov ecx, dword [rbx - mcb_size + mcb.blockSize]
    shl ecx, 4  ;Get number of bytes in the environment
    pop rbx
    return

getPtrToEndOfEnv:
;Gets ptr to end of the environment
;Output: rdi -> Second null byte of the terminator of the environment.
    push rax
    push rcx
    mov rdi, qword [r8 + psp.envPtr]
    xor eax, eax
    xor ecx, ecx
    dec ecx
.lp:
    repne scasb
    cmp byte [rdi], al
    jne .lp
    pop rcx
    pop rax
    return

growEnvBlock:
;Attempts to grow the environment to store the new string. Will clean the
; newly allocated 160 byte block of the environment.
;Returns:   CF=NC - Environment grown ok!
;           CF=CY & ZF=ZE - Environment at max size
;           CF=CY & ZF=NZ - Realloc failed.
    push rax
    push rbx
    push rcx
    push rsi
    push rdi
    call getFreeSpace       ;Get free space in environment in ecx
    call getPtrToEndOfEnv   ;Get ptr to free space in rdi
    mov rsi, qword [r8 + psp.envPtr]
    mov ebx, dword [rsi - mcb_size + mcb.blockSize]  ;Get current alloc size
    add ebx, 0Ah    ;Add 160 bytes!
    cmp ebx, 800h   ;Cannot be bigger than 8000h bytes
    jae .tooLarge
    ;Here ZF must be NZ.
    push r8
    mov r8, qword [r8 + psp.envPtr] ;Get the block ptr
    mov eax, 4A00h
    int 21h
    pop r8
    jc .exit
    ;Here now we must clean the new allocated region. We added 160 bytes
    ; so we add 160 zeros to the count of bytes
    add ecx, 0A0h
    xor eax, eax
    rep stosb
.exit:
    pop rdi
    pop rsi
    pop rcx
    pop rbx
    pop rax
    return
.tooLarge:
    xor rdi, rdi    ;Set ZF
    stc             ;Set CF
    jmp short .exit
