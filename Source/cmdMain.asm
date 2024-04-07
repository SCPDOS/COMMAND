commandStart:
    ;Resize Allocation, jump here with endpointer in rbx
    ;Ideally would have this jettisoned too but cannot guarantee
    ; that the jump to safety won't be gobbled up when multitasking
    neg r8  ;Convert r8 to -r8
    lea rbx, qword [rbx + r8]    ;Get # of bytes for COMMAND.COM and stack
    shr ebx, 4  ;Convert to paragraphs
    mov ah, 4Ah ;Realloc
    neg r8  ;Convert -r8 to r8
    int 21h
    jmp short commandMain
applicationReturn:  ;Return point from a task, all regs preserved
    mov eax, 4D00h ;Get Return Code
    int 21h
    mov word [returnCode], ax
;Reset our PSP vectors (and IVT copies) in the event they got mangled
    lea rdx, critErrorHandler
    mov qword [r8 + psp.oldInt24h], rdx
    mov eax, 2524h
    int 21h
    lea rdx, int23h
    mov qword [r8 + psp.oldInt23h], rdx
    mov eax, 2523h
    int 21h
    lea rdx, applicationReturn
    mov qword [r8 + psp.oldInt22h], rdx
    mov eax, 2522h
    int 21h
    test byte [pipeFlag], -1
    jnz commandMain.pipeProceed ;Skip the handle closing when pipe active
    call cleanUpRedir   ;Clean up redirection once we are done
;Close all handles from 5->MAX
    movzx ecx, word [numHdls]
    mov ebx, 5
.handleClose:
    mov ah, 3Eh ;File close
    int 21h
    inc ebx ;Goto next file
    cmp ebx, ecx
    jbe .handleClose    ;Keep looping whilst below or equal
commandMain:
    mov rsp, qword [stackTop]    ;Reset internal stack pointer pos
    cld ;Ensure stringops are done the right way
    mov byte [inBuffer], inBufferL    ;Reset the buffer length
.inputMain:
    call printCRLF
.inputMain2:
    call clearCommandLineState
    call printPrompt

    mov eax, 5D09h  ;Flush network printers
    int 21h
    mov eax, 5D08h  ;Set net printer state
    mov edx, 1      ;Start new print job
    int 21h
    lea rdx, inBuffer
    mov eax, 0A00h  ;Do Buffered input
    int 21h
    call printCRLF  ;Note we have accepted input
;First check we had something typed in of length greater than 0
;Must be greater than 0 as executable commands must have extension and filename
    cmp byte [inBuffer + 1], 0  ;Check input length valid
    je .inputMain2
    ;Copy over the input text
    lea rsi, inBuffer   ;This buffer is used for all input so copy command line
.copyPoint: ;Copy over commandline here
    lea rdi, cmdBuffer
    mov ecx, cmdBufferL   ;Straight up copy the buffer over
    rep movsb
.pipeLoop:
    mov r8, qword [pspPtr]  ;Ensure we have our pspPtr pointing to the right place
    call makeAEBuffer
    call analyseCmdline
    call doCommandLine
.pipeProceed:
    call cleanUpRedir
    test byte [pipeFlag], -1  ;If we have any pipes active, we proceed here
    jz .inputMain
    ;Now we pull the commandline forwards. 
    mov byte [redirNull], -1    ;Cleared by both cmdstate handlers
    call preProcessBuffer       ;Now find the pipe again
    cmp byte [rsi], CR  ;If this is so, exit!
    je .inputMain  ;No more pipes found so we are done with piping!
    ;rsi points to the start of the new string
    ;ecx = number of chars left unscanned
    ;rdi points to the command buffer
    lea rdi, cmdBuffer + 2  ;Get the start of the data in buffer
    mov rcx, rsi
    sub rcx, rdi    ;Get the difference i.e. the number of chars to remove
    sub byte [cmdBuffer + 1], cl    ;Minus from remaining chars
    mov cl, byte [cmdBuffer + 1]    ;Get this value 
    inc ecx         ;One more for the terminating char
    rep movsb       ;Move the chars over
    call clearCommandState  ;Else, clear the command state and start again
    jmp short .pipeLoop
.dfltErrExit:
    lea rdx, badCmd
    mov ah, 09h
    int 21h
    jmp .inputMain

makeAEBuffer:
;Make the AE buffer. Do this before everything to avoid messing 
; up internal vars! Done on each command executed. Im not happy
; with the way all this is implemented, I need to rewrite so that
; the redirNull flag is unnecessary. This will be best done by 
; unbinding the creating of the pipes and the registering of
; the redirection as that is what this flag essentially does.
; Possibly then make the cmdline preprocessor use the ae buffer 
; as the flexibility currently afforded is not worth it.

    mov byte [redirNull], -1
    mov word [aeBuffer], inBufferL  ;Zero the number of chars in buffer
    lea rdi, aeBuffer + 2
    push rdi
    call preProcessBuffer   ;Move rsi to the CR or the char past a pipe
.lp:
    pop rdi ;rdi points to where to copy the chars over to
    push rsi    ;Preserve the terminator here!
    lea rsi, qword [r8 + cmdLine]
    movzx ecx, byte [r8 + cmdLineCnt]   ;Get num of chars in buf here
    add byte [aeBuffer + 1], cl         ;Add num of chars copied here
    rep movsb
    pop rsi
    cmp byte [rsi], CR
    je .done
    mov al, "|"
    stosb   ;Store the pipe and advance the ptr
    inc byte [aeBuffer + 1] ;Add one more char!
    push rdi
    call preProcessBuffer.altEP ;Dont reset to the head of the buffer
    jmp short .lp
.done:
    lodsb
    stosb   ;Store the CR 
    mov byte [redirNull], 0
    return

preProcessBuffer:
;Start by preprocessing the path, escape quotes and handle redirections.
;Each normal char gets copied over to psp.dta + 1 except for those special chars.
;Places the count of chars save CR in byte 0 of psp.dta.
;Places the ptr to the first byte past pipe or CR in cmdEndPtr
;Throughout: CL has char count, CH has number of quotes.
    lea rsi, [cmdBuffer + 2]    ;Goto command buffer + 2
.altEP:
    xor ecx, ecx
    mov rdi, rsi    ;Save the pointer
.countQuotes:
    lodsb
    cmp al, '"'
    jne .notQuoteToCount
    inc ch      ;Keep count of number of quotes in ch
    jmp .countQuotes
.notQuoteToCount:
    cmp al, CR  ;Keep searching for quotes until CR hit
    jne .countQuotes
    mov rsi, rdi    ;Reset to the start of the buffer
    lea rdi, qword [r8 + cmdLine]   ;We store this nicely formatted string in psp
.getChar:
    lodsb           ;Get char in buffer, advance rsi
    cmp al, '"'     ;Is char a quote?
    jne .notQuote
    dec ch          ;We have a quote, now decrement
    jz .notQuote    ;If we have an odd count of quotes, ignore the last one!
.searchForClosingQuote:
    ;Char was quote, now directly store all chars until we hit closing char
    stosb   ;Store char and inc rdi
    inc cl
    lodsb   ;Get next char
    cmp al, '"'
    jne .searchForClosingQuote ;If not, keep searching directly
    dec ch  ;We hit another quote char, so dec the count
.notQuote:
    push rcx    ;Save counts, if anything goes wrong, stack is jiggled properly
    call checkAndSetupRedir ;Intervene redir
    pop rcx
    jnz .notRedir       ;Store the char as normal if not a redir
    jc .pipeHandle      ;Store a CR in the buffer if a pipe
    jmp short .getChar  ;Get the next char if a < or > redir
.pipeHandle:
    mov al, CR  ;Now store a Carriage return 
    inc rsi ;Ensure we remain one char past the pipe
.notRedir:
    stosb       ;Store char and advance rdi
    cmp al, CR  ;Was this char a CR?
    je .exit
    inc cl      ;Increment char count
    jmp short .getChar    ;If not, get next char
.exit:
    dec rsi ;Move back to the CR or the first char past the pipe
    mov byte [r8 + cmdLineCnt], cl  ;Store the count of chars in the psp buffer
    return  ;Returns rsi -> New path componant

analyseCmdline:
;Flags first two arguments if they exist, copies the command into its buffer
; processes the command name into the FCB.  
    call preProcessBuffer   ;Now preprocess and setup pipes!
    lea rsi, qword [r8 + cmdLine]   ;Go to the command line in the psp
    mov rbx, rsi            ;Save this ptr in rbx
    call skipDelimiters     ;Skip any preceeding separators
    lea rdi, cmdPathSpec    ;We copy the command name/path here
    call cpDelimPathToBufz  ;Moves rsi to the first char past the delim char
    dec rsi ;Point it back to the delim char
    call .skipAndCheckCR
    je .setupCmdVars
    mov byte [arg1Flg], -1  ;Set that we are 
    mov rax, rsi
    sub rax, rbx            ;rbx points to the start of the buffer
    mov byte [arg1Off], al  ;Store the offset 
.skipArg:
    lodsb   ;Now we advance the pointer to the second argument or CR
    cmp al, CR
    je .setupCmdVars
    call isALdelimiter
    jne .skipArg    ;If not a delimiter, get next char now
    call .skipAndCheckCR    ;Now skip all the delimiters
    je .setupCmdVars            ;If ZF set, this we encountered a CR
    mov byte [arg2Flg], -1  ;If it is not CR, it is a second argument!
    mov rax, rsi            
    sub rax, rbx            ;rbx points to the start of the buffer
    mov byte [arg2Off], al  ;Store the offset 
.setupCmdVars:
;Before returning, we copy the command name to cmdName 
    mov byte [cmdName], 0   ;Initialise this field to indicate no cmd
    lea rdi, cmdPathSpec
    call findLastPathComponant  ;Point rdi to last path componant
    call strlen ;Get the length of the final path componant
    cmp ecx, 11 + 1 ;Extra char for the ext separator (dot)
    ja .exitBad     ;Return error
    mov rsi, rdi
    lea rdi, cmdName
    dec ecx ;Minus the terminating null
    mov byte [rdi], cl ;Store the length here
    inc rdi ;Now goto next char in buffer
.cpCmdName:
    lodsb
    call ucChar ;Uppercase the char
    stosb
    dec ecx
    jnz .cpCmdName
    return
.exitBad:
    mov byte [cmdName], -1 ;Store -1 to indicate error
    return
.skipAndCheckCR:
;Skips all chars, rsi points to the separator. If it is a CR, set ZF=ZE
    call skipDelimiters ;Go to the next char in the input line
    cmp byte [rsi], CR  ;If it is not a CR, it is an argument
    return

doCommandLine:
    mov r8, qword [pspPtr]
    lea rsi, cmdPathSpec
    ;The following check accounts for the end of a piped command
    cmp byte [cmdName], 0  ;If the cmd name length is 0, fail!
    rete
    cmp byte [cmdName], -1  ;Error condition, command name too long!
    je badCmdError
    lea rdi, cmdFcb
    mov eax, 2901h  ;Skip leading blanks, clean the FCB name
    int 21h
    movzx ebx, word [cmdPathSpec]    ;Get the drive specifier
    cmp bh, ":"
    jne .noDriveSpecified
    xchg bl, al     ;Store drive status in bl, get letter in al
    call ucChar     ;Uppercase al
    sub al, "A"     ;And make it a 0 based drive letter
    cmp bl, -1      ;Int 21h returns AL = -1 if bad drive specified
    je .badDrive
    ;If drive specified and cmdName length = 2 => X: type command
    cmp byte [cmdName], 2
    jne .noDriveSpecified   ;Drive specified but proceed as normal
    mov dl, al  ;Setdrive wants the number in dl
    call setDrive
    rete
.badDrive:
    lea rdx, badDrv
    mov ah, 09h
    int 21h
    stc
    return
.noDriveSpecified:
;Now we set the two FCB's in the command line
    test byte [arg1Flg], -1
    jz .fcbArgsDone
    movzx eax, byte [arg1Off]   ;Get the first argument offset
    lea rsi, qword [r8 + cmdLine]
    add rsi, rax    ;Point to first argument
    lea rdi, qword [r8 + fcb1]
    mov eax, 2901h
    int 21h
    mov byte [arg1FCBret], al
    test byte [arg2Flg], -1
    jz .fcbArgsDone
    movzx eax, byte [arg2Off]
    lea rsi, qword [r8 + cmdLine]
    add rsi, rax    ;Point to first argument
    lea rdi, qword [r8 + fcb2]
    mov eax, 2901h
    int 21h
    mov byte [arg2FCBret], al
.fcbArgsDone:
    lea rbx, aeBuffer       ;Take your buffer, ergh
    lea rsi, cmdName        ;Point to command name with len prefix 
    mov eax, 0AE00h ;Installable command check
    mov edx, 0FFFFh
    mov ch, -1
    int 2Fh ;Give the TSR time to prepare if it needs to
    mov eax, 0AE00h ;Installable command check
    mov edx, 0FFFFh
    xor ch, ch  ;Second call uses ch = 0
    int 2Fh ;Return: al = -1 if this command a extension to COMMAND.COM
            ;        al = 0  if the command should be executed as usual
    test al, al
    jz .executeInternal
    ;Here we execute externally and return to the prompt
    ; as if it was an internal execution
    lea rbx, aeBuffer       ;Take your buffer, ergh
    lea rsi, cmdName        
    mov eax, 0AE01h ;Execute command!
    mov edx, 0FFFFh
    mov ch, -1
    int 2Fh
    cmp byte [cmdBuffer], 0 ;If this is non-zero, we execute internal
    jnz .executeInternal
    call cleanUpRedir       ;Close all redirs to reset!
    pop rsi                 ;Pop the return address off the stack
    lea rsi, aeBuffer       ;Point rsi to the 
    jmp commandMain.copyPoint   ;And copy the cmd over to execute!
.executeInternal:
;Now we check if the cmdName is equal to the length of the cmdPathSpec.
;If not, then its immediately an external program!
    lea rdi, cmdPathSpec
    call strlen ;Get the length of the input command
    dec ecx     ;Minus 1 for terminating null
    cmp byte [cmdName], cl  ;Is it equal to the name of the command?
    jne launchChild     ;If not, a path was specified, exit!
;Now we compare the name in the cmdName + 1 field to our commmand list
    lea rbx, functionTable
.nextEntry:
    movzx ecx, byte [rbx]   ;Get name entry length
    cmp cl, -1  ;Are we at the end of the table?
    je launchChild      ;If so, check externally now
    cmp byte [cmdName], cl  ;Is command length the same as the tbl entry length?
    jnz .gotoNextEntry  ;If not, goto next entry
    ;Here they have the same length so lets see if the name is the same
    push rsi
    ;ecx has the length to compare
    push rcx
    lea rsi, qword [rbx + 1]
    lea rdi, qword [cmdName + 1]   ;Go to the name portion
    rep cmpsb   ;Check the strings are equal
    pop rcx
    pop rsi
    jne .gotoNextEntry
    ;Here it was found both strings are equal
    lea rdi, qword [rbx + rcx + 1]  ;make rdi point to offset from startLbl
    movzx rbx, word [rdi]
    lea rdi, startLbl
    add rbx, rdi
    call rbx    ;Call this function...
    retc    ;Always return with CF=CY on error. Error code set to -1
    mov byte [returnCode], 0 ;Set the retcode to 0 if ok!
    return  ;... and return
.gotoNextEntry:
    add rbx, 3      ;Go past the first count byte and the address word
    add rbx, rcx    ;Go past the length of the command name too
    jmp short .nextEntry
.dfltErrExit:
    lea rdx, badCmd
    mov ah, 09h
    int 21h
    return

redirFailure:
    lea rdx, redirErrMsg
    mov ecx, redirErrMsgL
    jmp short redirPipeFailureCommon
pipeFailure:
    lea rdx, pipeErrMsg
    mov ecx, pipeErrMsgL
redirPipeFailureCommon:
;This routine is called if any problems happen during 
;This routine tries to close whatever handles are not -1 and delete
; pipe files if the pipe count is not 0
;It resets all variables and proceeds.
    mov eax, 4000h  ;Write handle
    mov ebx, 2  ;Write to STDERR
    int 21h
    xor ebx, ebx    ;Select STDIN
    call .closeHandle
    inc ebx         ;Select STDOUT
    call .closeHandle
    mov eax, 3D02h  ;Open read/write
    lea rdx, conName
    int 21h
    mov ebx, eax    ;Move file handle to ebx
    mov eax, 4500h  ;DUP
    int 21h
    mov word [redirIn], 0  ;Clear both flags
    movzx ebx, word [redirSTDIN]
    call .closeHandle
    ;Close and zero both STDIN and STDOUT handle vars
    mov word [redirSTDIN], -1
    movzx ebx, word [redirSTDOUT]
    call .closeHandle
    mov word [redirSTDOUT], -1
    movzx ebx, word [pipeSTDIN]
    call .closeHandle
    mov word [pipeSTDIN], -1
    movzx ebx, word [pipeSTDOUT]
    call .closeHandle
    mov word [pipeSTDOUT], -1
    mov byte [pipeFlag], 0  ;Cover the pipe number too
    lea rdx, qword [pipe1Filespec]
    cmp byte [rdx], 0
    jz .checkOld
    mov eax, 4100h  ;Del File pointed to by rdx
    int 21h
.checkOld:
    lea rdx, qword [pipe2Filespec]
    cmp byte [rdx],0
    jz .pipeNamesComplete
    mov eax, 4100h  ;Del File pointed to by dl
    int 21h
.pipeNamesComplete:
    xor eax, eax
    ;Invalidate the pointers and the paths too
    mov qword [newPipe], rax
    mov qword [oldPipe], rax
    mov dword [pipe1Filespec], eax
    mov dword [pipe2Filespec], eax
    stc
    return
.closeHandle:
    cmp ebx, -1
    rete
    mov eax, 3E00h
    int 21h
    return

cleanUpRedir:
;Cleans up the redir stuff after we are done.
    movzx eax, word [pipeSTDIN]
    movzx ebx, word [pipeSTDOUT]
    shl ebx, 10h
    or eax, ebx
    cmp eax, -1
    jne .pipe
    mov byte [pipeFlag], 0  ;Clear the flag
    jmp .redirInClear   ;If no piping, skip
.pipe:
;Pipe processing here
;We handle stdin, closing the redir if it is and deleting
; the redir file.
;Then we handle stdout, moving the redir to stdin.
    cmp word [pipeSTDIN], -1
    je .pipeNostdin
    ;We close the handle first and delete the file.
    movzx ebx, word [pipeSTDIN] 
    xor ecx, ecx    ;DUP into STDIN closing the redir
    mov eax, 4600h
    int 21h
    jc pipeFailure
    mov eax, 3E00h  ;Close the copy
    int 21h
    jc pipeFailure
    mov rdx, qword [oldPipe]    ;Get the ptr to the filename
    mov eax, 4100h  ;Delete the file!
    int 21h
    jc pipeFailure
    mov byte [rdx], 0           ;Mark this buffer as free
    mov word [pipeSTDIN], -1    ;This has been closed now
.pipeNostdin:
    cmp word [pipeSTDOUT], -1   ;If no stdout redir, exit now
    je .redirInClear
;Duplicate STDIN to save across pipe
    mov eax, 4500h
    xor ebx, ebx    ;Set ebx to STDIN
    int 21h
    jc pipeFailure
    mov word [pipeSTDIN], ax    ;Save duplicate here

;Now move STDOUT to STDIN, closing original STDIN in the process
    mov eax, 4600h
    mov ecx, ebx    ;DUP STDOUT into STDIN
    inc ebx ;ebx = 1, ecx = 0
    int 21h
    jc pipeFailure

;Now return the original stdout to stdout
    mov ecx, ebx
    movzx ebx, word [pipeSTDOUT]
    mov eax, 4600h  ;ebx = pipeSTDOUT, ecx = 1
    int 21h
    jc pipeFailure

;Now close the DUP'ed STDOUT
    mov eax, 3E00h
    int 21h
    jc pipeFailure

;Finally unwind STDIN to the beginning of the file
    mov eax, 4200h  ;Seek from start of file
    xor ebx, ebx    ;STDIN handle
    mov ecx, ebx    ;Set high 32 bits
    mov edx, ebx    ;Set low 32 bits
    int 21h
    jc pipeFailure  ;This should never happen

    mov rdx, qword [newPipe]    ;Move the pathname pointer
    mov qword [oldPipe], rdx
    mov word [pipeSTDOUT], -1   ;Set this to free for next use
.redirInClear:
;Check redir in
    test byte [redirIn], -1
    jz .redirOutClear

    movzx ebx, word [redirSTDIN]    ;Put this file back to STDIN
    xor ecx, ecx    ;Duplicate original STDIN into CX (into STDIN position)
    mov eax, 4600h  ;This closes the redir file in the process
    int 21h
    jc redirFailure
    mov eax, 3E00h  ;Now close BX in the process too remove duplicates.
    int 21h
    jc redirFailure
    mov word [redirSTDIN], -1  ;Replace the file handle with -1
    mov byte [redirIn], 0   ;Clear the flag
.redirOutClear:
;Now check Redir Out
    test byte [redirOut], -1
    retz    ;Return if not set

    movzx ebx, word [redirSTDOUT]    ;Put this file back to STDOUT
    mov ecx, 1    ;Duplicate original STDOUT into CX (into STDOUT position)
    mov eax, 4600h  ;This closes the redir file in the process
    int 21h
    jc redirFailure
    mov eax, 3E00h  ;Now close BX in the process too remove duplicates.
    int 21h
    jc redirFailure
    mov word [redirSTDOUT], -1  ;Replace the file handle with -1
    mov byte [redirOut], 0   ;Clear the flag
    return

checkAndSetupRedir:
;Checks and sets up redir as appropriate
;Input: al = First char to check, if al < > >> or |, handled appropriately
;       rsi points to the first char after the char in al in cmdBuffer
;Output: ZF=NZ => No redir
;        ZF=ZY => Redir
;           rsi is moved to the first non-terminating char after redir filespec
;CF=CY if pipe set or an embedded CR found. rsi points to first char past it!
    push rdi
    cmp al, "<"
    je .inputRedir
    cmp al, ">"
    je .outputRedir
    cmp al, "|"
    je .pipeSetup
    clc
.redirExit:
    pop rdi
    return
.inputRedir:
    lea rdi, rdrInFilespec
    call .ensureRightBuffer
    call skipDelimiters ;Skip spaces between < and the filespec
    call cpDelimPathToBufz
    dec rsi ;Point rsi back to the delimiter char as 
    test byte [redirNull], -1   ;If redirNull set then dont act!
    jnz .redirNull
    ;Setup the redir here for STDIN
    test byte [redirIn], -1
    jnz .redirError
    mov byte [redirIn], -1  ;Set the redir in flag
    xor ebx, ebx    ;DUP STDIN
    mov eax, 4500h
    int 21h
    jc .redirError
    mov word [redirSTDIN], ax   ;Save the handle in variable
    lea rdx, rdrInFilespec
    mov eax, 3D02h  ;Open file for read write access
    int 21h
    jc .redirError
    xor ecx, ecx    ;Close STDIN and duplicate bx into it
    movzx ebx, ax   ;Move the handle into bx to duplicate into cx (STDIN)
    mov eax, 4600h
    int 21h
    jc .redirError
    mov eax, 3E00h  ;Now close the original copy of the handle
    int 21h
    jc .redirError
    xor al, al
    jmp .redirExit
.outputRedir:
    mov ebx, 1  ;Set this as the flag
    cmp byte [rsi], ">" ;Was this a > or a >>
    jne .notDouble
    inc ebx ;Inc to make it 2
    inc rsi ;Go past it too
.notDouble:
    lea rdi, rdrOutFilespec
    push rbx
    call .ensureRightBuffer
    call skipDelimiters
    call cpDelimPathToBufz
    pop rbx
    dec rsi ;Point rsi back to the delimiter char as 
    test byte [redirNull], -1   ;If redirNull set then dont act!
    jnz .redirNull
    test byte [redirOut], bl    ;If there is already an out, fail!
    jnz .redirError 
    mov byte [redirOut], bl
    ;Setup the redir here for STDOUT
    mov ebx, 1    ;DUP STDOUT
    mov eax, 4500h
    int 21h
    jc .redirError
    mov word [redirSTDOUT], ax   ;Save the handle in variable
    lea rdx, rdrOutFilespec
    mov eax, 3D02h  ;Open file for read write access
    int 21h
    jnc .fileExists
    mov eax, 3C00h
    xor ecx, ecx  ;Make the file with no attributes
    int 21h
    jc .redirError
.fileExists:
    mov ecx, 1      ;Close STDOUT and duplicate bx into it
    movzx ebx, ax   ;AX has the new handle for output
    mov eax, 4600h  ;DUP2
    int 21h
    jc .redirError
    mov eax, 3E00h  ;Now close the original copy of the handle (in bx)
    int 21h
    jc .redirError
    cmp byte [redirOut], 1
    je .dontAppend
    ;Here we move the file pointer to the end of the file
    xor edx, edx    ;Low order 32 bits
    xor ecx, ecx    ;High order 32 bits
    mov ebx, 1  ;We seek STDOUT to the end
    mov eax, 4202h  ;Seek from end of file
    int 21h
    jc .redirError
.dontAppend:
    mov byte [redirOut], -1
.redirNull:
    xor al, al
    jmp .redirExit
.pipeSetup:
;We only need to setup STDOUT redirection to the pipe file
    push rsi    ;Save rsi pointing to char past |
    call skipDelimiters ;Check if this is a double ||
    cmp byte [rsi], "|" 
    pop rsi
    je .pipeError
    test byte [redirNull], -1   ;If redirNull set then dont act!
    jnz .pipeNull
    lea rdx, pipe1Filespec
    cmp byte [rdx], 0
    jz .pathFound
    lea rdx, pipe2Filespec
    cmp byte [rdx], 0
    jnz .pipeError
.pathFound:
    mov qword [newPipe], rdx    ;Use this as the newPipe path
    mov eax, 4500h  ;Now DUP STDOUT
    mov ebx, 1
    int 21h
    jc .pipeError
    mov word [pipeSTDOUT], ax   ;Save the copy of the handle
    call getCurrentDrive    ;Get current drive in al (0 based number)
    add al, "A"
    mov ebx, 005C3A00h  ;0,"\:",0
    mov bl, al  ;Move the drive letter into low byte of ebx
    mov dword [rdx], ebx    ;Put the \ terminated path
    xor ecx, ecx      ;Hidden attributes
    mov eax, 5A00h  ;Create a temporary file
    int 21h
    jc .pipeError
    ;AX has the handle for this file now, this will become STDOUT
    ;If this is the first pipe, we want to save a copy of this handle
    movzx ebx, ax   ;Clone new handle into STDOUT
    mov ecx, 1
    mov eax, 4600h
    int 21h
    jc .pipeError
    mov eax, 3E00h  ;Now close the original copy of the handle (in bx)
    int 21h
    jc .pipeError
    mov byte [pipeFlag], -1 ;Set the pipe flag up!
.pipeNull:
    xor al, al  ;Set ZF
    stc         ;But also CF to indicate pipe!
    pop rdi
    return
.pipeError:
    pop rdi 
    call pipeFailure
    jmp commandMain ;Fully reset the state if a pipe failure occurs.
.redirError:
    pop rdi 
    call redirFailure
    jmp commandMain ;Fully reset the state if a redir failure occurs.
.ensureRightBuffer:
    test byte [redirNull], -1   ;If redirNull not set then act!
    retz
    lea rdi, searchSpec ;Use searchSpec as a nul instead
    return


int2Eh:   ;Interrupt interface for parsing and executing command lines
;Input: rsi points to the count byte of a command line
    push r8
    push r9
    mov ah, 51h ;Get Current PSP in rdx
    int 21h
    push rdx    ;Save on the stack
    lea rbx, qword [startLbl - psp_size]    ;Get a psp ptr for this COMMAND.COM
    mov ah, 50h ;Set this version of COMMAND.COM as the current PSP
    int 21h
    mov r8, rbx ;Set to point to the command.com psp
    mov r9, rbx
    lea rdi, qword [r8 + cmdLine]
    mov ecx, 10h    ;7Fh chars + 1 count byte / 8
    rep movsq   ;Copy command line over
    ;call doCommandLine
    pop rbx ;Get Old current PSP in rbx
    mov ah, 50h ;Set Current PSP
    int 21h
    pop r9
    pop r8
    iretq