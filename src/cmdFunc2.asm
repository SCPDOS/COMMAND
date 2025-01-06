;The second file with internal functions.
;Functions included with name and label:
;----------------------------------------------------
; LABEL         NAME
;----------------------------------------------------
; type          TYPE
; exit          EXIT
; launchChild   <LAUNCH CHILD>
; set           SET
; pathEdit      PATH
; prompt        PROMPT
; echo          ECHO
; pauza         PAUSE
; remark        REM 
; shift         SHIFT
; goto          GOTO 
; ifCmd         IF  
; forCmd        FOR
;----------------------------------------------------

type:
    test byte [arg1Flg], -1 ;If this not set, error
    jz badArgError
    test byte [arg2Flg], -1
    jnz badArgError         ;If this set, error
    mov r8, [pPSP]
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
    ;If it is a char device, don't print an error
    mov eax, 4400h  ;Get IOCTL mode 
    mov ebx, 1
    int 21h
    test dl, devCharDev
    retnz
    jmp badDiskFull

exit:
    test byte [statFlg1], permaShell
    jz .ouch        ;If not permashell, time to EXIT
    test byte [statFlg1], inSingle  ;If perma and single command
    jnz int2ERet    ;We are Int 2Eh, exit through the handler
    return  ;Else, permashell just returns
.ouch:
    mov eax, 4C00h  ;Exit now okay
    int 21h
    return  ;If the exit wasn't successful for some reason, return as normal

launchChild:
    ;We execute an external command here.
    ;Here we will behave like COMMAND.COM for later DOS and honour extensions.
    ;COMMAND.COM on DOS 3.3 doesn't honour the extension. If there exists a 
    ;foo.com and a foo.exe in the same dir and you type foo.exe it launches 
    ;foo.com. We will not honour this behaviour as this kinda sucks!
    ;We know the drive we are on is valid so no need to double check that!      
    ;Now we rebuild the cmdFcb from the last path componant.
    lea rdi, cmdPathSpec
    mov rsi, rdi
    mov eax, 1211h  ;Normalise this path first
    int 2fh
    call findLastPathComponant  ;Point rdi to last path componant
    mov rsi, rdi    ;Source here
    lea rdi, cmdFcb
    mov eax, 2901h  ;Skip leading blanks, clean the FCB name
    int 21h
    test al, al     ;Don't allow any wildcards in the name
    jnz badCmdError
    xor rbp, rbp    ;rbp keeps a ptr to the next PATH componant to search in
.pathLoop:
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
    je batLaunch
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
;If the program failed to start, verify why!
    mov eax, 5900h      ;Get extended error
    xor ebx, ebx
    int 21h
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; The below doesnt make any sense. Retcode is
; is not the DOS error code.
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ;mov word [returnCode], ax   ;Error code from EXEC
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    cmp al, errAccDen   ;Access denied?
    je badAccError
    cmp al, errNoMem    ;If not enough memory, print so
    je badNoMemError    
    cmp al, errMCBbad   ;If MCB bad error, freeze PC
    je freezePC
    jmp badCmdError     ;If something goes wrong, error out
.pathHandle:        
;First check if rbp is null. If it is, its a first time entry. 
;al has error code!
    test rbp, rbp
    jnz .pathReentry
;Now check if the command we recieved included an absolute path.
;If so, we don't do path substitution and just fail at this point
    lea rsi, cmdPathSpec
    lodsw   ;Get the first two chars
    cmp al, byte [pathSep]  ;If char 1 a pathsep, we are absolute!
    je badCmdError  ;Therefore, exit bad!
    test ah, ah ;If the second char is nul, its a 1 char command, must be rel
    je .pathGetEnv
    cmp ah, ":" ;If char 2 is colon, must be drive sep (not chardev)
    jne .pathGetEnv ;Therefore, if not equal, relative path!
    lodsb   ;Get the third char
    cmp al, byte [pathSep]  ;If this is a pathsep, we are absolute!
    je badCmdError  ;Therefore, exit bad!
.pathGetEnv:
;Now get a pointer to the environment and search for PATH=
;We only take the final portion of the path and add it to each path componant
;This agrees with DOS 3.3
    call checkEnvGoodAndGet   ;Ensure our env is double null terminated!
    jz badCmdError  ;If returned ZF=ZE, error out!
    ;If we are here, env is double null terminated. rsi has the env ptr
    ;Now we know we dont have to keep track of chars!!
    lea rdi, pathEVar   ;Get a ptr to the path env string
    call searchForEnvVar
    jc badCmdError      ;If PATH not found, exit error!
    mov rdi, rsi        ;Move the PATH= ptr to rdi
    add rdi, 5          ;Go past the PATH= portion of the env string
.pathRejoin:
    cmp byte [rdi], 0   ;Is the first char after equals a null?
    je badCmdError      ;Empty or no more path? Error!
    mov rsi, rdi        ;This is a ; or null delimited ASCII string
    lea rdi, searchSpec ;Build the path in searchSpec
    ;WARNING!!! THIS COULD CAUSE A BUFFER OVERFLOW BUG!!
    ;SHOULD CHECK THE LENGTH OF THE PATH COMPONANT THAT WE ARE 
    ; SPLICING ON. IF IT IS LONGER THAN 64 CHARS WE IGNORE IT!!
    ;This is impossible to do if env edited by COMMAND.COM
    call cpDelimOrCtrlStringToBufz      ;Copies upto ; or null 
    dec rsi ;Point rsi to the char which delimited the path
    mov rbp, rsi    ;Point rbp to this char
    dec rdi ;Point to the null terminator
    mov al, byte [pathSep]
    cmp byte [rdi - 1], al
    je .skipPathsep ;Need for rootdir as a double slash start is a netpath
    stosb   ;Store a pathsep onto the null terminator
.skipPathsep: ;Affects double slashes in path (but thats ok)
    lea rsi, qword [cmdFcb + fcb.filename]
    call FCBToAsciiz    ;Store the name here and null terminate
    lea rsi, searchSpec 
    lea rdi, cmdPathSpec
    call strcpy         ;Copy the string to the cmdPathSpec
    jmp .pathLoop       ;And try again, now in this path!
.pathReentry:
    cmp byte [rbp], 0   ;Each env string is finally null terminated.
    je badCmdError
;Currently, because we dont swap drives, we wont print this message.
;Not really a big deal...
    ;cmp al, errBadDrv
    ;jne .pathDrvOk
    ;lea rdx, badDrvSrch ;Print the drive was invalid!
    ;mov eax, 0900h
    ;int 21h
;.pathDrvOk:
    inc rbp             ;Go to the start of the next componant
    mov rdi, rbp        ;So rdi points to the first char of next comp
    jmp short .pathRejoin   ;Check if null, and if not, proceed again!

.prepAndSearch:
;Copies over the name and extension in UC to the last componant of the 
;cmdPathSpec and null terminates. 
;Input: cmdFcb name + ext setup. 
;Output: rdx -> Filled in cmdPathSpec 
;        CF=NC, file in rdx found. CF=CY, file in rdx not found! al = errcde
    push rcx
    push rsi
    push rdi
    lea rdi, cmdPathSpec 
    mov rdx, rdi    ;Save the path ptr in rdx
    call findLastPathComponant  ;Point rdi to the final path componant 
    cmp byte [rdi + 1], ":"
    jne .notdriveRel
    add rdi, 2  ;Go past the drive specifier, but keep it!
.notdriveRel:
    lea rsi, qword [cmdFcb + fcb.filename]
    call FCBToAsciiz    ;Get an asciiz suffix
    mov eax, 4E00h  ;Find first
    xor ecx, ecx  ;Only Normal and RO files searchable!
    int 21h
    pop rdi
    pop rsi
    pop rcx
    return

set:
    test byte [arg1Flg], -1
    jnz .editEnv
    ;Here we just print the environment.
    call checkEnvGoodAndGet 
    jz badEnvSpaceError
    ;We know this is a good env so keep going! env ptr in rsi
    mov rdi, rsi
    mov rdx, rsi
.findLp:
    mov ecx, -1
    xor eax, eax
.scanLp:
    repne scasb
    jne .scanLp
    not ecx ;Get count and subtract by 1 to drop end null
    ;Else, we now print this environment variable and CRLF it
    ;rdx points to the start of the string aleady
    ;ecx has the bytes to print
    mov ebx, 1      ;Print to STDOUT
    mov eax, 4000h  ;Print to handle
    int 21h
    call printCRLF
    cmp byte [rdi], 0   ;Is this a second null.
    rete    ;Return if it is
    mov rdx, rdi   
    jmp short .findLp
.editEnv:
    call checkEnvGoodAndGet
    jz badEnvSpaceError
    movzx eax, byte [arg1Off]
    lea rsi, qword [r8 + cmdLine]
    add rsi, rax            ;rsi -> EnvvarName=[string]<CR>
.altEp:
    mov rdi, rsi            ;Point rdi to the start of the string
    mov al, CR              ;Search for the CR
    movzx ecx, byte [r8 + cmdLineCnt]   ;Get the char count of the tail
    mov ebx, ecx            ;Save this count 
    push rbx                ;Save this count on the stack too!
    repne scasb             ;Now get the length of this env string
    sub ebx, ecx            ;Get the length of the string with <CR>
    mov word [envVarSz], bx ;Store the count
    pop rcx                 ;Get the char count back!
    mov ebx, ecx            
    mov rdi, rsi            ;Point rdi again to the cmdTail
    mov al, "="             ;Scan for an equals sign
    repne scasb
    jne badSyntaxError      ;There must be an equal sign here!
    sub ebx, ecx            ;Get the count with space for a terminating 0
    mov word [envVarNamSz], bx
    cmp byte [rdi], CR      ;If the path is just PATH=<CR>, free var!
    je .delEnvVar           ;Free the envvar and return
    push rsi                ;Save the ptr to the start of the envvar
    mov rdi, rsi            ;Input= rdi -> String to search for
    call searchForEnvVar    ;Look for the environment variable
    jc .editNewEnv          ;Create a new environment variable!
    call envFree            ;Free the var in rsi
.editNewEnv:
    call getFreeSpace       ;Get the free space in env in ecx
    call getPtrToEndOfEnv   ;Point to the free space in rdi
    pop rsi                 ;Get back the ptr to the string
    xor eax, eax            ;Prepare null length!
    cmp ecx, 4              ;Minimal env string size is 4, i.e. "X=A<CR>"
    cmovb ecx, eax          ;If below, essentially 0 bytes
    jb .reallocTry          ;Try to reallocate!
    dec ecx                 ;Save a byte for end magic null
    cmp cx, word [envVarSz] ;Do we have enough space?
    jae .nameCp             ;If above or equal, we good to go!
.reallocTry:
    ;Here we try to reallocate the environment. If we cannot reallocate
    ; we proceed with the environment size.
    call growEnvBlock   ;Attempt to grow the environment
    jnc .reallocOk
    jz badEnvSpaceError    ;Max environment space
    ;Here we couldnt reallocate, but we check to see if we can fit
    ; partially the variable into the env. If so we do that. If the name
    ; cannot fit, we don't bother
    ;Min size of envVarNamSz is 2 i.e. "X="
    cmp cx, word [envVarNamSz]  ;cx has the free space in environment
    retb    ;Fail silently if we cant, catches the cx=0,1 case!
    call badEnvSpaceError   ;Print the env space error!
    dec ecx  ;Make space for the end null of the environment! Wont overflow!
    jmp short .nameCp   ;Else, just copy what we can
.reallocOk:
    ;Here we have all the space to just copy the full path
    movzx ecx, word [envVarSz]
.nameCp:
;Now copy over the env var, ecx = #ofchars to copy
;Always enough memory for name= here
    lodsb
    call ucChar
    stosb
    dec cx      ;Always decrement this count
    cmp al, "="
    jne .nameCp
.exitNameCp:
    lodsb
    cmp al, CR
    je .exitCp
    stosb
    dec ecx
    jnz .exitNameCp
.exitCp:
    xor eax, eax
    stosw   ;Store the final null word
    return
.delEnvVar:
    ;rsi -> Start of the envvar name in user buffer
    mov rdi, rsi            ;Input= rdi -> String to search for
    call searchForEnvVar    ;Look for the environment variable
    jc .delEnvVarSkip
    call envFree    ;Free the env var at rsi
.delEnvVarSkip:
    clc             ;Clear CF if the var doesnt exist
    return

pathEdit:
;Each path componant !!must!! be terminated by a semicolon. 
;No separators allowed either side of the semicolon. Spaces allowed ONLY 
; after the equals sign. If what follows a semicolon is a terminator, we 
; end there. Error with too many arguments error!
;If after the equals sign there is nothing, we just print the path!
;Gotta process the command line ourselves first
    call checkEnvGoodAndGet         ;Use as a good environment check!
    jz badEnvSpaceError
    ;Now we know we can use r8 to get the envptr when needed
    lea rsi, qword [r8 + cmdLine]   ;Get the ptr to scan for ; or <CR>
    call .skipPathDelimiters
    cmp al, CR
    je .printPath
;Else use the set command to setup the path in the searchSpec.
    push rsi    ;Save the first char of the command line string
    lea rsi, pathEVar
    lea rdi, searchSpec
    call strcpy
    dec rdi     ;Point to the terminating null
    pop rsi     ;Get back the first char of the pathstring
    xor ecx, ecx    ;Make a count of chars
.cpLp:
    lodsb
    cmp al, CR
    je .cpLpExit
    call .isALPathDelimiter ;Any path delims now are exit conditions!
    je .cpLpExit
    call ucChar ;Uppercase the char
    stosb       ;Store it
    inc ecx     ;Inc the count, CR not inclusive!
    jmp short .cpLp
.cpLpExit:
    ;Remove trailing semicolons
    cmp byte [rdi - 1], ";" ;Check back a char
    jne .cpLpNoIntervene
    dec ecx     ;Reduce the count by one
    dec rdi     ;Go back a char
    jmp short .cpLpExit ;Keep removing
.cpLpNoIntervene:
    mov al, CR  ;Now we store the terminator!
    stosb       ;Store it!
    add ecx, 5  ;PATH= is 5 chars
    mov byte [r8 + cmdLineCnt], cl  ;Store the count
    inc ecx     ;Add the CR for copying over
    lea rsi, searchSpec
    lea rdi, qword [r8 + cmdLine]
    push rdi
    rep movsb
    pop rsi         ;Get the ptr to the cmdline in rsi
    jmp set.altEp   ;We've now set the string up, lets go!
.printPath:
    lea rdi, pathEVar   ;This is what we want to get
    call searchForEnvVar    ;Returns ptr to env var in rsi
    jc .noPathPrnt      ;If the var doesnt exit, print no path!
    mov rdi, rsi        
    call strlen         ;Get the length of the path in rdi
    cmp ecx, 6          ;Is our path just PATH=<NUL>?
    je .noPathPrnt      ;Print no path!
    dec ecx             ;Drop the terminating null from the count
    mov rdx, rdi        ;Set path ptr for printing
    mov ebx, 1          ;STDOUT
    mov eax, 4000h      ;ecx = char count, rdx points to PATH= string
    jmp short .pathExit
.noPathPrnt:
    lea rdx, noPath
    mov eax, 0900h
.pathExit:
    int 21h
    call printCRLF      ;Print a crlf at the end
    return
.skipPathDelimiters:
;Input: rsi -> Start of string to parse
;Output: rsi -> First non-delimiter char of string
;        al = First non delim char
    lodsb
    call .isALPathDelimiter
    je .skipPathDelimiters
    dec rsi
    return
.isALPathDelimiter:
;Same as before without semicolon
    cmp al, SPC
    rete
    cmp al, TAB
    rete
    cmp al, ","
    rete
    cmp al, "="
    return

prompt:
    call checkEnvGoodAndGet         ;Use as a good environment check!
    jz badEnvSpaceError
    ;Now we know we can use r8 to get the envptr when needed
    lea rsi, qword [r8 + cmdLine]   ;Get the ptr to scan for ; or <CR>
    call skipDelimiters ;Points rsi to the first non-delimiter char
    cmp byte [rsi], CR  ;Is the first non-delim a CR?
    je .reset   ;Reset if so
    push rsi    ;Save ptr to the start of the user typed line
    lea rsi, promptEVar ;Copy the PROMPT= prefix to searchspec
    lea rdi, searchSpec
    mov ecx, 7  ;Copy without <CR>
    rep movsb
    pop rsi ;Get back ptr to the user typed line
    xor ecx, ecx    ;Get char count
.cp:    ;Now copy the user string over
    lodsb
    stosb 
    cmp al, CR
    je .cpOk
    inc ecx ;Increment count if non CR char copied over
    jmp short .cp
.cpOk:
    lea rsi, searchSpec ;Source the string from here
    jmp short .goSet    ;Got the CR-less count
.reset:
;Delete the environment variable!
    lea rsi, promptEVar 
    xor ecx, ecx    ;No chars to write to the envstring
.goSet:
    add ecx, 7  ;Add the chars for the prompt= string too
    mov byte [r8 + cmdLineCnt], cl 
    inc ecx     ;Include CR in copy
    lea rdi, qword [r8 + cmdLine]
    push rdi
    rep movsb
    pop rsi
    jmp set.altEp

echo:
    test byte [arg1Flg], -1 ;If no argument, display if on or off
    jnz .argGiven
    lea rdx, echoIs
    call printString
    lea rdx, onMes
    lea rcx, offMes
    test byte [echoFlg], -1
    cmovz rdx, rcx
    jmp printString
.argGiven:
    lea rsi, qword [r8 + cmdLine]
    movzx eax, byte [arg1Off]   ;Get the offset
    add rsi, rax
    lodsb   ;Get this char
    dec rsi ;And go back to the start of the string
    call ucChar
    cmp al, "O" ;Was it an O? If not, direct copy
    jne .directEcho
    mov al, byte [rsi + 1]  ;Get the next char
    call ucChar
    cmp al, "N" ;If its N, check its the last char on the string
    jne .checkOff
    push rsi
    add rsi, 2  ;Go past on string
    call skipDelimiters
    cmp byte [rsi], CR
    pop rsi
    jne .directEcho ;If its not, just echo the string
    mov byte [echoFlg], 1   ;Set to 1 if on
    return
.checkOff:
    mov al, byte [rsi + 1]  ;Get first char past O
    call ucChar
    cmp al, "F" ;Is it an F?
    jne .directEcho ;No, just direct echo
    mov al, byte [rsi + 2]
    call ucChar
    cmp al, "F"
    jne .directEcho
    push rsi
    add rsi, 3
    call skipDelimiters
    cmp byte [rsi], CR
    pop rsi
    jne .directEcho
    mov byte [echoFlg], 0
    return
.directEcho: 
    lea rdx, qword [r8 + cmdLine]
    mov rbx, rsi    
    sub rbx, rdx
    movzx ecx, byte [r8 + cmdLineCnt]   ;Get original char count
    sub ecx, ebx    ;Get the remaining chars
    jc printCRLFecho    ;If something weird, echo nothing
    mov rdx, rsi
    mov ebx, 1
    mov eax, 4000h
    int 21h
    jmp printCRLF   ;Needs to be a proper CRLF to insert a CRLF at the end!

pauza:  ;Well... pause is an instruction in english 0:)
;Thank you authors of MSDOS Encyclopedia for confusing an argument to this command
; with just... the actual command tail being echoed with the command -_-
    test byte [echoFlg], -1
    jnz .echoTail
;Since we havent echoed the command out, type the tail out manually
    lea rsi, qword [r8 + cmdLine]  ;Goto command line
    call skipDelimiters ;Skip leading delims
    mov rdx, rsi
    movzx ecx, byte [r8 + cmdLineCnt]  ;Get the count
    mov ebx, 1  ;Echo to STDOUT 
    mov eax, 4000h  ;Write
    int 21h
    call printCRLF
.echoTail:
    lea rdx, pauseMes
    call printString
    mov eax, 0800h  ;CON input w/o echo. Triggers ^C
    int 21h
    call printCRLF
    return
remark:
;If in a batch file, do nothing. Else, go through normal loop.
    test byte [statFlg1], inBatch
    retz
.go:
    pop rbx
    pop rbx ;Realign the stack back :)
    call getSetMainState
    jmp commandMain.inputGetAgain   ;Clean any redirs and get input

shift:
;If not in batch, immediately return!
    test byte [statFlg1], inBatch
    retz
    mov rbx, qword [bbPtr]  ;Get the batch block
    lea rdi, qword [rbx + batBlockHdr.wArgs]
    lea rsi, qword [rdi + 2]    ;Source from one word ahead
    mov ecx, 9
    xor eax, eax
.lp:
    lodsw
    stosw
    cmp eax, 0FFFFh   ;Once we xfer a -1 word, no more args on cmd line
    retz
    dec ecx
    jnz .lp
;Now we gotta scan for one more cmdline argument
    mov word [rbx + batBlockHdr.wArgs + 2*9], -1   ;Init a -1 at the end
    mov al, CR
    xor ecx, ecx
    dec ecx ;
    lea rdi, qword [rbx + rax]  ;rax has the last offset
    repne scasb ;Find CR which terminated old last argument, go past it
    cmp byte [rdi], 0   ;If this is the terminating null, leave as -1
    rete
    sub rdi, rbx    ;Get the offset from batBlockHdr
    mov word [rbx + batBlockHdr.wArgs + 2*9], di    ;Store the difference
    return
    
goto:
;If not in batch, immediately return!
    test byte [statFlg1], inBatch
    retz
    mov rbp, qword [bbPtr]
    test rbp, rbp
    retz
;Start by copying the command line label to fcb1
    lea rsi, qword [r8 + cmdLine]
    lea rdi, qword [r8 + fcb1 + fcb.filename]  ;Use fcb1 for the command line
    call skipDelimiters     ;Go to the first argument on cmdline
    mov ecx, 8
    cmp byte [rsi], ":" ;If we the first char of the cmdline lbl is :, skip
    jne .startCopy
    inc rsi
.startCopy:
    lodsb
    cmp al, SPC         ;Skip any spaces
    je .startCopy       
    cmp al, CR          ;If CR, exit copy
    je .endCopy
    call isALdelimiter  ;If delimiter char, exit copy
    jz .endCopy
    stosb           
    dec ecx             ;Decrement counter
    jnz .startCopy
.endCopy:
;Now search the batch file for the label.
    neg ecx
    add ecx, 8      ;Get the number of chars copied into ecx
    lea rdx, qword [r8 + fcb1]
    mov byte [rdx + fcb.driveNum], cl    ;Store the count in drivenum
    call .ucChars   ;Now we UC the chars in the string
    mov qword [rbp + batBlockHdr.qBatOff], 0    ;Reset the file ptr
    call batOpen    ;Open the batch file. Handle in ebx.
;File opened from the start. Now start byte by byte read.
.notLabelLp:
    test byte [statFlg1], batchEOF  
    jnz .eof    ;If we hit an ^Z while processing file, don't loop again
    lea rdx, [r8 + fcb2 + fcb.filename]  ;fcb2 for the bat search buffer
.findLbl:
;Keep searching for a label
    call batReadChar
    jz .eof
    inc qword [rbp + batBlockHdr.qBatOff] ;Inc the fp for each char read
    cmp byte [rdx], ":"
    jne .findLbl 
;Here we found a candidate label. Take 8 chars w/o spaces and initial :
    xor ecx, ecx
.loadRead:
    call batReadChar
    jz .lblDone
    mov al, byte [rdx]  ;Get the char read into al
    inc qword [rbp + batBlockHdr.qBatOff] ;Inc the fp for each char read
    cmp al, CR
    je .lblDoneCR
    cmp al, LF
    je .lblDone
    cmp al, SPC
    je .loadRead
    call isALdelimiter
    jz .pullEol ;If we have a delimiter char, pull it
    inc rdx ;Inc the storage pointer
    inc ecx ;Inc the count
    cmp ecx, 8  ;Once we read 8 chars, readthru to end of line
    jne .loadRead
.pullEol:
    call batReadChar
    jz .lblDone
    inc qword [rbp + batBlockHdr.qBatOff] ;Inc the fp for each char read
    cmp byte [rdx], CR
    je .lblDoneCR
    cmp byte [rdx], LF
    je .lblDone
    jmp short .pullEol
.lblDoneCR:
;Read a CR, check if the next char is an LF and scan past it.
    call batReadChar
    jz .lblDone
    cmp byte [rdx], LF
    jne .lblDone    ;No LF
;Else include the LF in the count to go past
    inc qword [rbp + batBlockHdr.qBatOff] 
.lblDone:
;Check what we have to see if it is possible to form a label
    lea rdx, qword [r8 + fcb2]
    mov byte [rdx + fcb.driveNum], cl  ;Store the len in the drive letter
    call .ucChars    ;Now we UC the chars in the fcb pointed to by rdx
;Now compare the strings (trailing space padding)
    lea rsi, qword [rdx]
    lodsb   ;Get the count into al and move rsi to filename
    movzx ecx, al   ;Move the count into ecx
    lea rdi, qword [r8 + fcb1 + fcb.filename]
    cmp byte [rdi - 1], cl  ;If the counts are not equal, skip the cmp
    jne .notLabelLp
    repe cmpsb  ;Now do a string cmp
    jne .notLabelLp
    ;Here if the label is found. Bat FP points to the next line to read.
    call batClose   ;Close the handle
    jmp remark.go   ;Now behave like rem to get the next line!
.eof:
;Print label not found, end batch mode and return
    lea rdx, badLbl
    call printString
    call batFinish  ;Kill the batch processor
    return

.ucChars:
;Input: rdx -> Buffer where the first byte gives number of chars to UC
;Output: The rdx[0] bytes from rdx[1] are UC'd
    push rax
    push rcx
    push rsi

    lea rsi, qword [rdx + 1]    ;Start of string to uppercase
    movzx ecx, byte [rdx]       ;Get byte count to uppercase
.ucclp:
    lodsb   ;Get the char
    call ucChar
    mov byte [rsi - 1], al  ;Replace the char with it's UC'd version
    dec ecx     
    jnz .ucclp  ;Go again if we havent exhausted all chars
    
    pop rsi
    pop rcx
    pop rax
    return

ifCmd:
    mov byte [ifFlg], ifReset ;Reset not state
    lea rsi, qword [r8 + cmdLine]
    call getNextArg    ;Skip leading delimiters
    mov rbx, rsi        ;Save the possible start of string ptr (if string)
    call makeAsciizAdv ;Move rsi to next word, rdi -> ASCIZ string
    push rsi    ;Save ptr to the next word on stack
    lea rsi, notString
    call strcmp
    pop rsi
    jne .chkErlvl
    or byte [ifFlg], ifNot  ;Set not on
    mov rbx, rsi    ;Save the start of string ptr (if string)
    call makeAsciizAdv     ;Goto next word
.chkErlvl:
    push rsi    ;rsi points to the argument
    lea rsi, errlvlStr
    call strcmp
    pop rsi
    je .errorLvl
    push rsi
    lea rsi, existStr
    call strcmp
    pop rsi
    je .exist
;Here we check condition string1==string2
;rsi points to the start of the string to check condition of
    mov rsi, rbx    ;Get back the start of the string
    mov rdi, rsi    ;Move rdi to the start of the string
    xor ecx, ecx    ;String length cnt
.scCheck:
    lodsb
    cmp al, "="
    je .scEqFnd
    call isALdelimiter
    je badSyntaxError
    cmp al, CR
    je badSyntaxError
    inc ecx         ;One more char to count
    jmp short .scCheck
.scEqFnd:
    lodsb   ;Move rsi to the char past this equal sign
    cmp al, "=" ;Is the second char an equal too?
    jne badSyntaxError
    repe cmpsb  ;Compare the strings, leave rsi past string 2
    jnz .cndMiss
.cndHit:
    or byte [ifFlg], ifCond ;The default condition was hit
.cndMiss:
    movzx eax, byte [ifFlg]
    mov ebx, eax
    shr ebx, 1      ;Get bit 1 to bit 0
    and eax, 1      ;Isolate bit 0
    xor eax, ebx    ;xor the condition hit bit with not. If 1, execute!
    retz            ;Else return silently!
;Now rsi points to delims before the command. 
; Skip the delims and copy the argument!
    call skipDelimiters    ;Now go to the next argument (No need for CR check)
    lea rdi, qword [cLineBuffer + 2]
    xor ecx, ecx
.cpExitLp:
    lodsb
    stosb
    inc ecx ;Add a new char to the count
    cmp al, CR
    jne .cpExitLp
    dec ecx ;Drop CR from count
    mov byte [cLineBuffer + 1], cl
    pop rax ;Balance the stack
    pop rax
    jmp commandMain.batProceed    ;And execute the command now!
.exist:
;Here we do the check for file existance
    call makeAsciizAdv
    mov ecx, dirDirectory    ;Search for normal, RO and dir
    mov rdx, rdi    ;Move the ptr to rdx
    mov eax, 4E00h  ;Find first
    int 21h
    jnc .cndHit
    jmp short .cndMiss
.errorLvl:
;Here we do the check for error level
    call makeAsciizAdv
    xchg rdi, rsi
    call getNum     ;Get value in eax
    cmp eax, 255    ;Value can't be bigger than 255
    ja badSyntaxError
    xchg rdi, rsi
    cmp al, byte [returnCode]
    je .cndHit
    jmp .cndMiss

forCmd:
;FOR %<var> IN (list) DO command
;Allocate a FOR block, parse the for command line and fill in the forBlk.
; Works by writing a new version of the commandline for each element in the
; list. If %<arg> in the command, it just executes the command n times where 
; n is the number of elements in the list.
    test byte [forFlg], -1
    jnz badForError
    mov ebx, ((forBlk_size + 0Fh) >> 4) ;Get paras to allocate
    mov eax, 4800h  ;ALLOC
    int 21h
    jc badNoMemError
    mov byte [forFlg], -1   ;Set var
    mov qword [pForBlk], rax    ;Save the ptr to the ForBlk
    mov rbp, rax            ;Move forblk ptr to rbp
;Clean the memory block for use
    mov rdi, rax
    xor eax, eax
    mov ecx, forBlk_size
    rep stosb
;Clean any pre-established redirs
    call batKillRedir   ;Preserves rbp
;Now copy the command line to the block :)
    lea rsi, cLineBuffer   ;Start reading what we typed in
    lea rdi, qword [rbp + forBlk.sCmdLine]
    push rdi
    mov ecx, cmdBufferL
    rep movsb   ;And copy!
    pop rsi     ;Now source the command line from our copy! :)
;Now we parse the command line.
    add rsi, 2  ;Now skip the buffer length bytes
    call getNextArg     ;Moves rdi to the FOR. Guaranteed to be so! 
.findForLp:
    call makeAsciizAdv  ;Move rsi to the next word (%<VAR>) [LAZY!]
    push rsi
    lea rsi, forStr
    call strcmp
    pop rsi
    jne .findForLp  ;We are guaranteed to have a FOR in the command line
    call makeAsciizAdv  ;Moves rdi to the buffer. rsi to IN
    cmp byte [rdi], "%"
    jne .forBadSynExit
    cmp byte [rdi + 2], 0
    jne .forBadSynExit
    movzx eax, byte [rdi + 1]
    mov byte [rbp + forBlk.bLpVar], al  ;Store the loopchar
    call makeAsciizAdv  ;Moves rdi to the buffer. rsi to "("
    push rsi
    lea rsi, inStr
    call strcmp
    pop rsi
    jne .forBadSynExit
;Need special handling now as "(" might be appended to element
    lodsb   ;Get the byte, advance rsi past it 
    cmp al, byte [listOpenStr]
    jne .forBadSynExit
    call getNextArg
    ;rsi points to the first list element
.argCpy:
    call makeAsciizAdv  ;Move rsi to the next list element
;Now we check if this is just a ")"
    movzx eax, word [rdi]   ;Get this word
    cmp ax, word [listClosStr]  ;Was this ")"<NUL> ?
    je .argCpyEnd
;Here we have a list element, rsi points to next entry in list
    inc byte [rbp + forBlk.bListc]  ;We have one more argument
    mov rbx, qword [rbp + forBlk.pLstCurr]  ;Get ptr to space for list element
    test rbx, rbx
    jnz .inProgress
;Here we have the first list element
    lea rbx, qword [rbp + forBlk.sListBlk]   ;Start writing here
.inProgress:
;Now check if this command is terminated with a ). If it is, end of list.
    push rdi
    call strlen ;Get the string length 
    sub ecx, 2     ;Drop terminating null
    add rdi, rcx    ;Point to the final char
    movzx eax, word [rdi]   ;Get this char
    cmp ax, word [listClosStr]  ;Was this ")"<NUL> ?
    jne .notEnd
    mov byte [rdi], 0   ;Overwrite with a null over the ")"
.notEnd:
    pop rdi
    push rsi
    mov rsi, rdi    ;Source ASCIZ from the buffer we built in
    mov rdi, rbx    ;Write here
    call strcpy     ;Copy and advance pointers with terminating null
    mov qword [rbp + forBlk.pLstCurr], rdi  ;Store next arg here
    pop rsi
    cmp ax, word [listClosStr]  ;Check again
    jne .argCpy ;If not equal, we loop again
.argCpyEnd:
;rsi points to DO command here. 
;Parsed all arguments, store ptr to head of asciiz list for processing
    call makeAsciizAdv  ;Move rsi to the command string
    push rsi
    lea rsi, doStr
    call strcmp
    pop rsi
    jne .forBadSynExit
    mov qword [rbp + forBlk.pCmd], rsi  ;Store ptr to head of cmd string :)
    mov qword [rbp + forBlk.pLstCurr], 0 ;Signal to start from first arg
    cmp byte [rbp + forBlk.bListc], 0   ;If the count of args 0, syntax error
    je .forBadSynExit
    call printCRLF  ;Else print CRLF to indicate command accepted 
    jmp short forProceed    ;And go!!!!
.forBadSynExit:
    call forFree
    jmp badSyntaxError

forProceed:
;Start by getting the forblk pointer and set the FFblock immediately
    mov rbp, qword [pForBlk]    ;Get the ptr to the for block
    lea rdx, qword [rbp + forBlk.sFFBuffer]
    mov eax, 1A00h  ;Set the DTA to the FFblock dta
    int 21h
;Now we ascertain whether or not we need a new list entry or use existing
.longString:
    test byte [rbp + forBlk.bCmdWC], -1 ;If entry has WC, keep using
    jne .ctnFor
;Here we go for a new entry! Check if we are at the start of the list.
    test qword [rbp + forBlk.pLstCurr], -1    ;If null, just starting
    jnz .getNextListElement
;Here if we are starting a new for command.
    lea rsi, qword [rbp + forBlk.sListBlk]  ;First arg is here
    jmp short .useString
.getNextListElement:
;Else get the ptr and advance it!
    mov byte [rbp + forBlk.bCmdWC], 0   ;Turn off wildcard if on
    mov rsi, qword [rbp + forBlk.pLstCurr]  ;Get last processed arg ptr
    mov rdi, rsi    ;Need for strlen
    call strlen
    add rsi, rcx    ;Goto next arg!
    movzx ecx, byte [rbp + forBlk.bListc]
    inc byte [rbp + forBlk.bArgNum] ;We've gone to the next arg
    cmp byte [rbp + forBlk.bArgNum], cl
    je forEnd  ;Once equal, we have processed all args. Game over!
.useString:
;Now update the pointer in the block!
    mov qword [rbp + forBlk.pLstCurr], rsi  ;Now working on this arg.
;Now scan string for wildcards
    push rsi
.wcCheck:
    lodsb
    test al, al 
    jz .wcCheckEnd
    cmp al, "*"
    je .wcFnd
    cmp al, "?"
    jne .wcCheck
.wcFnd:
    mov byte [rbp + forBlk.bCmdWC], -1  ;We have wildcards!
.wcCheckEnd:
;Here we copy the argument to the buffer (for possible expansion)
    pop rsi ;Pop back the head of the string to copy over
    lea rdi, qword [rbp + forBlk.sNameBuf]
    call strcpy2    ;Copy w/o moving the pointers
;rsi now can be trashed!
    mov ecx, dirDirectory
    mov eax, 4E00h  ;Find first
.searchAgain:   ;Need rdi -> Path for below
    mov rdx, rdi    ;Move the pointer to rdx for search. 
    int 21h 
    jc .getNextListElement  ;Get next list element.
    cmp byte [rbp + forBlk.bCmdWC], -1  ;If no WC, use sNameBuf as is!
    jne .copyCommand
;Else, we must replace the wildcards. Cannot be in path as find first
; doesn't resolve wildcards in path componants, only filename.
    call findLastPathComponant  ;Point rdi to the last path componant
;We don't need rsi pointing to the string anymore.
    lea rsi, qword [rbp + forBlk.sFFBuffer + ffBlock.asciizName]
    call strcpy2    ;Copy w/o moving pointers
    jmp short .copyCommand
.ctnFor:
;If we searched a filespec with a wildcard, we come here for the next
; in the series. forBlk.sNameBuf is not corrupted while executing a command
    mov eax, 4F00h  ;Find Next file for the file found in FFBuffer
    lea rdi, qword [rbp + forBlk.sNameBuf]  ;Need for getting last pathcomp
    jmp short .searchAgain  ;We have a WC, get next file!
.copyCommand:
;We substitute the string in forBlk.sNameBuf into the command line when we
; hit a matching %<var>
    mov rsi, qword [rbp + forBlk.pCmd]  ;Copy the command line
    lea rdi, qword [cLineBuffer + 2]   ;We will be writing to this buffer
    xor ecx, ecx    ;Keep track of chars we copy over
.ccLp:
    lodsb
    cmp al, "%"     
    je .ccMeta      ;Go if metachar found!
.ccLp1:
    cmp al, CR
    jz .ccLpEnd     ;Exit if CR terminator found!
    cmp ecx, inLen  ;If we are at space for CR w/o it then string too long.
    je .ccLpEnd     ;Truncate here
    inc ecx         ;Else, add one more char
    stosb           ;And shove it!
    jmp short .ccLp
.ccMeta:
    lodsb           ;Get the next char
    cmp al, byte [rbp + forBlk.bLpVar]  ;Compare if this is a var
    jne .ccLp1  ;If not a var, strip the % and store the char directly
;Else, here we expand!
    push rsi
    push rdi
    mov edx, ecx    ;Save current char count in edx
    lea rdi, qword [rbp + forBlk.sNameBuf]
    call strlen     ;Get replacement char count to ecx
    dec ecx         ;Drop the terminating null from the count
    add edx, ecx    ;Get their sum
    cmp edx, inLen  ;Position 128 is saved for CR. sum must be less
    jb .ccOk
    pop rdi
    pop rsi
    jmp .longString     ;Get the next argument!
.ccOk:
    mov rsi, rdi    ;Source from the name buffer
    pop rdi         ;Now get the original position to write the string in
    rep movsb       ;Copy over the string
    pop rsi         ;And keep sourcing cha
    mov ecx, edx    ;Get the current count of chars back into ecx
    jmp short .ccLp ;And get the next char
.ccLpEnd:
    stosb       ;Store the terminating CR
    mov byte [cLineBuffer + 1], cl ;Store var count here
    jmp commandMain.goSingle    ;And do it! :)
forEnd:
    lea rsi, qword [rbp + forBlk.sCmdLine]
    lea rdi, cLineBuffer
    mov ecx, cmdBufferL
    rep movsb   ;Zoom zoom copy the cmdline back home :)
    call forFree
    lea rsp, stackTop   ;Reset stack ptr! Unlikely needed!
;Since we flushed buffers and reset cmdprocessor state before entering
; we can jump directly to get input. There cannot be any redirections as
; redirections are interpreted as part of the command that was looped on.
    jmp commandMain.inputGetAgain

forPrintCmd:
    pushfq
    test byte [forFlg], -1
    jz .exit    ;If not in a for, return
    call printPrompt    ;Now output prompt
    lea rdx, qword [r8 + cmdLine]   ;Get pre-pull cmdline!
    movzx ecx, byte [rdx - 1]    ;Get the number of chars to print
    mov ebx, 1  ;STDOUT
    mov eax, 4000h  ;Write woo!
    int 21h
    call printCRLFecho  ;Only print if ECHO ON. Note we have accepted input!
.exit:
    popfq
    return

forFree:
;Reset FOR state. Frees the for block and clears the vars.
    push rax
    push r8
    mov r8, qword [pForBlk]
    test r8, r8
    jz .exit
    mov eax, 4900h  ;Free block!
    int 21h
.exit:
    xor eax, eax
    mov qword [pForBlk], rax
    mov byte [forFlg], al
    pop r8
    pop rax
    return

callCmd:
    ;jmp badCmdError ;Currently report bad command :)
    lea rsi, qword [r8 + cmdLine]   ;Copy the command tail!
    lea rdi, cLineBuffer + 2
    movzx ecx, byte [r8 + cmdLineCnt]  ;Get the count of chars
    mov byte [rdi - 1], cl
    inc ecx         ;Include the CR in the copy!
    rep movsb
    mov byte [callFlg], -1  ;In a call!
    call batKillRedir       ;Liquidates all redirs and deletes presetup files
    pop rax                 ;Realign stack
    pop rax
    jmp commandMain.batProceed

callClean:
;Frees everything but the last one. 
;There must be a pointer in bbPtr!
;Returns with rbx -> Last pointer
    mov rbx, qword [bbPtr]
    mov rax, qword [rbx + batBlockHdr.pLink]
    test rax, rax
    retz
    push rax    ;Save the prev ptr
    call batFree
    pop qword [bbPtr]   ;Pop it into var
    jmp short callClean
    