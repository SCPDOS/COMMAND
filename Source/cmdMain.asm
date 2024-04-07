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
    call advanceRedir   ;Clean up redirection once we are done
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
    mov byte [inBuffer], inBufferL      ;Reset the buffer length
    mov byte [cpyBuffer], inBufferL     ;Reset the buffer length
    mov byte [cmdBuffer], inBufferL     ;Reset the buffer length
.inputMain:
    call printCRLF
.inputMain2:
    test byte [batFlag], -1 ;If batch on, go here
    jnz batNextLine
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
    cmp byte [inBuffer + 1], 0  ;Check input length valid
    je .inputMain2
    ;Copy over the input text
    lea rsi, inBuffer   ;This buffer is used for all input so copy command line
.copyPoint: ;Copy over commandline here
    lea rdi, cpyBuffer
    mov ecx, cmdBufferL   ;Straight up copy the buffer over
    rep movsb
    call makeCmdBuffer   ;Preprocess the redir, make cmd buffer
.pipeLoop:
    mov r8, qword [pspPtr]  ;Point back to home segment
    call makeCmdString      ;Now make the command string in the psp
    call setupRedirandPipes ;Setup pipes and redir if appropriate
    call analyseCmdline
    call doCommandLine  ;This analyses and does the command line!
.pipeProceed:
    call advanceRedir
    test byte [pipeFlag], -1  ;If we have any pipes active, we proceed here
    jz .inputMain
    ;Now we pull the commandline forwards. 
    call makeCmdString  ;Get offset into cmdBuffer + 2 of pipe in rsi
    jz .inputMain       ;We are pointing to a CR, no pipe!
    lea rdi, cmdBuffer + 2
    mov rcx, rsi
    sub rcx, rdi    ;Get the number of chars to erase from cmd line 
    sub byte [cmdBuffer + 1], cl    ;And rease from the count
    mov cl, byte [cmdBuffer + 1]
    inc ecx     ;One more for the terminating char
    rep movsb   ;Move the chars over    
    call clearCommandState  ;Else, clear the command state and start again
    jmp short .pipeLoop

makeCmdBuffer:
;Makes the command buffer, escapes quotes and peels off any redirs from the
; copy buffer. Called only once in a cycle.
;Throughout: CL has char count, CH has quote count
    lea rsi, [cpyBuffer + 2]    ;Goto copy buffer + 2
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
    lea rdi, cmdBuffer + 2   ;We build the cmdBuffer
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
    call peelRedir      ;Intervene redir if needed
    jc .pipeHandle      ;Store the pipe if pipe
    jnz .notRedir       ;Store the char as normal if not a redir
    jmp short .getChar  ;Get the next char if a < or > redir
.pipeHandle:
    ;Store the pipe char, al has the char. IF ZF=ZE, we error
    jz pipeFailure ;We had double pipe symbol, reset!
.notRedir:
    stosb       ;Store char and advance rdi
    cmp al, CR  ;Was this char a CR?
    je .exit
    inc cl      ;Increment char count
    jmp short .getChar    ;If not, get next char
.exit:
    mov byte [cmdBuffer + 1], cl  ;Store the count of chars
    return

makeCmdString:
;Makes the command string from the cmdBuffer. This only copies to the first 
; pipe or the CR. Points at the CR or to the first char past the pipe.
; Copies into the psp.
;If we return ZF=ZE, then we are at end of pipe buffer. Else, setup pipe!
    lea rsi, [cmdBuffer + 2]    ;Goto pipe buffer + 2
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
    lea rdi, qword [r8 + cmdLine]   ;We build the single command
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
    cmp al, "|"
    jne .notPipe        ;Store the char as normal if not a pipe
    mov al, CR          ;Store the pipe char
    inc rsi             ;Ensure we remain one char past the pipe
.notPipe:
    stosb       ;Store char and advance rdi
    cmp al, CR  ;Was this char a CR?
    je .exit
    inc cl      ;Increment char count
    jmp short .getChar    ;If not, get next char
.exit:
    dec rsi ;Point to char past pipe or to the CR itself
    mov byte [r8 + cmdLineCnt], cl  ;Store the count of chars
    cmp byte [rsi], CR  
    return

analyseCmdline:
;Flags first two arguments if they exist, copies the command into its buffer
; processes the command name into the FCB.  
    lea rsi, qword [r8 + cmdLine]   ;Go to the command line in the psp
    mov rbx, rsi            ;Save this ptr in rbx
    call skipDelimiters     ;Skip any preceeding separators
    lea rdi, cmdPathSpec    ;We copy the command name/path here
    call cpDelimPathToBufz  ;Moves rsi to the first char past the delim char
    dec rsi ;Point it back to the delim char
    call .skipAndCheckCR
    je .setupCmdVars
    ;breakpoint
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
    lea rbx, cmdBuffer       ;Take your buffer, ergh
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
    lea rbx, cmdBuffer       ;Take your buffer, ergh
    lea rsi, cmdName        
    mov eax, 0AE01h ;Execute command!
    mov edx, 0FFFFh
    mov ch, -1
    int 2Fh
    cmp byte [cmdName], 0 ;If this is non-zero, we restart the process
    retz    ;Return as normal if this is zero
    ;Else, we restart the command from scratch.
.executeInternal:
;Now we check if the cmdName is equal to the length of the cmdPathSpec.
;If not, then its immediately an external program!
    call pullCommandline    ;Now pull the tail down
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

redirFailure:
    lea rdx, redirErrMsg
    mov ecx, redirErrMsgL
    jmp short redirPipeFailureCommon
pipeFailure:
    lea rdx, pipeErrMsg
    mov ecx, pipeErrMsgL
redirPipeFailureCommon:
;This routine is called or jumped to if any problems happen during a pipe.
;This routine tries to close whatever handles are not -1 and delete
; pipe files if the pipe count is not 0.
;It resets all variables and jumps to reset the stack!
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

    jmp commandMain ;Retake input from the command line
.closeHandle:
    cmp ebx, -1
    rete
    mov eax, 3E00h
    int 21h
    return

advanceRedir:
;Cleans up the redir stuff after we are done. Advances the pipe.
;Close stdin redir then stdout redir. 
    test byte [redirIn], -1     ;If the flag is set proceed.
    jnz .redirIn
    test byte [pipeFlag], -1    ;Is the pipe on?
    jz .redirOut                ;If not, check redir out
    movsx eax, word [pipeSTDIN] ;Do we have an incoming pipe to clean?
    cmp eax, -1   ;If the handle is -1, it must be an stdout pipe.
    je .pipeOut              
;Here we clean pipeSTDIN. Close the handle first then delete the file.
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
    ;Now we check the out pipe. If it is on, jump to it!
    ;Else, check if the redir out is on. If it is, jump to it. 
    ;Else exit!
    movsx eax, word [pipeSTDOUT]
    cmp eax, -1
    jne .pipeOut                ;If this is non-empty, go for it!
    mov byte [pipeFlag], 0      ;Turn off the pipe now!
    test byte [redirOut], -1    ;Do we have a redir out?
    jnz .redirOut               ;Go for it if so!
    return
.redirIn:
;Here we clean redirIn
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
    test byte [pipeFlag], -1    ;Is the pipe on? Check pipe out!
    jnz .pipeOut
.redirOut:
    test byte [redirOut], -1    ;If no redirOut, exit!
    retz
    ;If it is set, but the pipe is also set, the pipe takes presedence
    test byte [pipeFlag], -1
    jnz short .pipeOut
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
.pipeOut:
;Duplicate STDIN to save across pipe
    movsx eax, word [pipeSTDOUT] ;Do we have an outgoing pipe to clean?
    cmp eax, -1   ;If the handle is -1, no
    rete
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
    return


peelRedir:
;Checks and sets up redir as appropriate
;Input: al = First char to check, if al < > >> or |, handled appropriately
;       rsi points to the first char after the char in al in cpyBuffer
;Output: ZF=NZ => No redir
;        ZF=ZY => Redir
;           rsi is moved to the first non-terminating char after redir filespec
;CF=CY if pipe set. If ZF=ZE then double pipe, error!
    push rcx
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
    pop rcx
    return
.inputRedir:
    mov byte [redirIn], -1  ;Set the flag, redir in active
    lea rdi, rdrInFilespec
.redirCommon:
    call skipDelimiters ;Skip spaces between < > or >> and the filespec
    call cpDelimPathToBufz
    dec rsi ;Point rsi back to the delimiter char
    xor al, al  ;Clear the ZF flag
    jmp short .redirExit
.outputRedir:
    mov byte [redirOut], 1  ;Set this as the flag
    cmp byte [rsi], ">" ;Was this a > or a >>
    jne .notDouble
    inc byte [redirOut] ;Inc to make it 2
    inc rsi ;Go past it too
.notDouble:
    lea rdi, rdrOutFilespec
    jmp short .redirCommon
.pipeSetup:
    push rsi    ;Save rsi pointing to char past |
    call skipDelimiters ;Check if this is a double ||
    cmp byte [rsi], "|" 
    pop rsi
    stc
    jmp short .redirExit

setupRedirandPipes:
;We only need to setup STDOUT redirection if the pipe flag is set.
;Input: ZF=NZ => Set up pipes. ZF=ZE => just redir
    jz .redirIn
    lea rdx, pipe1Filespec
    cmp byte [rdx], 0
    jz .pathFound
    lea rdx, pipe2Filespec
    cmp byte [rdx], 0
    jnz pipeFailure
.pathFound:
    mov qword [newPipe], rdx    ;Use this as the newPipe path
    mov eax, 4500h  ;Now DUP STDOUT
    mov ebx, 1
    int 21h
    jc pipeFailure
    mov word [pipeSTDOUT], ax   ;Save the copy of the handle
    call getCurrentDrive    ;Get current drive in al (0 based number)
    add al, "A"
    mov ebx, 005C3A00h  ;0,"\:",0
    mov bl, al  ;Move the drive letter into low byte of ebx
    mov dword [rdx], ebx    ;Put the \ terminated path
    xor ecx, ecx      ;Hidden attributes
    mov eax, 5A00h  ;Create a temporary file
    int 21h
    jc pipeFailure
    ;AX has the handle for this file now, this will become STDOUT
    ;If this is the first pipe, we want to save a copy of this handle
    movzx ebx, ax   ;Clone new handle into STDOUT
    mov ecx, 1
    mov eax, 4600h
    int 21h
    jc pipeFailure
    mov eax, 3E00h  ;Now close the original copy of the handle (in bx)
    int 21h
    jc pipeFailure
    mov byte [pipeFlag], -1 ;Set the pipe flag up!
.redirIn:
;Now check if any other redir needs to be setup
;Checks and sets up redir as appropriate.
    ;Setup the redir here for STDIN
    test byte [redirIn], -1  ;Is it set
    jz .redirOut
    xor ebx, ebx    ;DUP STDIN
    mov eax, 4500h
    int 21h
    jc redirFailure
    mov word [redirSTDIN], ax   ;Save the handle in variable
    lea rdx, rdrInFilespec
    mov eax, 3D02h  ;Open file for read write access
    int 21h
    jc redirFailure
    xor ecx, ecx    ;Close STDIN and duplicate bx into it
    movzx ebx, ax   ;Move the handle into bx to duplicate into cx (STDIN)
    mov eax, 4600h
    int 21h
    jc redirFailure
    mov eax, 3E00h  ;Now close the original copy of the handle
    int 21h
    jc redirFailure
.redirOut:
    test byte [pipeFlag], -1    ;If pipe is active, pause setting redir out
    retnz
    test byte [redirOut], -1    ;Is redir out on?
    retz                        ;Exit if not
    ;Else setup the redir here for STDOUT
    mov ebx, 1    ;DUP STDOUT
    mov eax, 4500h
    int 21h
    jc redirFailure
    mov word [redirSTDOUT], ax   ;Save the handle in variable
    lea rdx, rdrOutFilespec
    mov eax, 3D02h  ;Open file for read write access
    int 21h
    jnc .fileExists
    mov eax, 3C00h
    xor ecx, ecx  ;Make the file with no attributes
    int 21h
    jc redirFailure
.fileExists:
    mov ecx, 1      ;Close STDOUT and duplicate bx into it
    movzx ebx, ax   ;AX has the new handle for output
    mov eax, 4600h  ;DUP2
    int 21h
    jc redirFailure
    mov eax, 3E00h  ;Now close the original copy of the handle (in bx)
    int 21h
    jc redirFailure
    cmp byte [redirOut], 1
    rete
    ;Here we move the file pointer to the end of the file
    xor edx, edx    ;Low order 32 bits
    xor ecx, ecx    ;High order 32 bits
    mov ebx, 1  ;We seek STDOUT to the end
    mov eax, 4202h  ;Seek from end of file
    int 21h
    jc redirFailure
    return


pullCommandline:
;This command pulls the command tail down, removing the whole launch command
    lea rsi, qword [r8 + cmdLine]  ;rbx points to the de-redired command line 
;Skip leading separators
.pctSkipLeading:
    lodsb   ;Get first char
    call isALdelimiter
    je .pctSkipLeading
    dec rsi
    ;rsi points to the start of the command
    lea rdi, cmdPathSpec
    call strlen ;Get the length of the command
    dec ecx ;Minus the terminating null
    ;breakpoint
    add rsi, rcx    ;Now move rsi to the first char past the command name
    sub byte [arg1Off], cl  ;Reduce these counts by the same amount!
    sub byte [arg2Off], cl
    xor ecx, ecx    ;Use as a char counter
    lea rdi, qword [r8 + cmdLine]    ;First byte is reserved for count
.pctPullChars:
    lodsb
    stosb
    cmp al, CR  ;Was this a terminating CR?
    je .pctExit
    inc ecx     ;Increment count
    jmp short .pctPullChars 
.pctExit:
    mov byte [r8 + cmdLineCnt], cl  ;Save the count
    return

batNextLine:
;Not yet implemented, if this is set to 1, print error and reset!
    lea rdx, .l1
    mov eax, 0900h
    int 21h
    mov byte [batFlag], 0
    jmp commandMain
.l1 db "Batch mode... wait, what? How did you do that?",CR,LF,"$"

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