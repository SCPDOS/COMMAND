commandStart:
;Jumped to with edx=0 means no autoexec. edx=-1 means autoexec.
;Now resize the allocation
    lea rbx, endOfAlloc
    sub rbx, r8 ;Get # of bytes for COMMAND.COM and stack
    add ebx, 0Fh    ;Round up paragraph
    shr ebx, 4  ;Convert to paragraphs
    mov eax, 4A00h ;Realloc
    int 21h
    test edx, edx   ;If zero, no autoexec or single command
    jz commandMain
    call getSetMainState    ;Ensure the buffers are ready to be used 
;If inSingle is set, cmd already in buffer and ready for processing  
    test byte [statFlg1], inSingle
    jnz commandMain.goSingle 
    ;Only enter here if we are autoexec :)
    mov byte [cLineBuffer + 1], autoSpecL - 1  ;Drop one from the count for CR
    lea rsi, autoSpec
    lea rdi, cLineBuffer + 2
    call strcpy
    mov byte [rdi - 1], CR  ;Store a CR over the terminating null
    jmp commandMain.batProceed
commandMain:
    lea rsp, stackTop   ;Reset internal stack pointer pos
    call getSetMainState
.inputMain:         ;Only reset once per line!
    test byte [statFlg1], inSingle   ;If we here in single mode, time to exit
    jnz exit
    call printCRLFecho  ;Command complete, indicate with new line!
    mov eax, 5D09h  ;Flush network printers
    int 21h
    mov eax, 5D08h  ;Set net printer state
    mov edx, 1      ;Start new print job
    int 21h
.inputGetAgain:
    call clearCommandLineState      ;Cleans all handles 5->MAX
.inputGetCmdlineAgain:
    test byte [forFlg], -1  ;If we are in a FOR loop, proceed with for
    jne forProceed
    test byte [statFlg1], inBatch   ;If batch on, get the next line to execute
    jnz batNextLine
    mov byte [callFlg], 0   ;If we need to get input from cmdline, no more call!
    call printPrompt    ;Ok we are gonna get more input, output prompt
    lea rdx, inBuffer
    mov eax, 0A00h      ;Do Buffered input
    int 21h
;Copy over the input text
    lea rsi, inBuffer       ;Preserve what was input
    lea rdi, cLineBuffer
    mov ecx, cmdBufferL     ;Copy the buffer over to manipulate
    rep movsb
.batProceedCrlf:
    call printCRLF      ;Note we have accepted input
.batProceed:            ;Jump here to copy the batch input line 
;First check we had something typed in of length greater than 0
    cmp byte [cLineBuffer + 1], 0  ;Check input length valid
    je .inputGetCmdlineAgain  ;If not, keep looping input
.goSingle:
    call makeCmdBuffer      ;Preprocess the redir, make cmd buffer
    ;Now check we aren't starting with a pipe or <CR> and treat differently
    lea rsi, cmdBuffer + 2
    call skipDelimiters
    cmp byte [rsi], CR      ;If the first non-delim is a CR, reject input!
    je .inputGetAgain       ;Wipe redir flags and reobtain input!
    cmp byte [rsi], "|"     ;If the first non-delim is a pipe, syntax error!
    je hardSynErr
.pipeLoop:
    mov r8, qword [pPSP]  ;Point back to home segment
    call makeCmdString      ;Makes the CR delimited command in psp
    ;ZF here indicates if we are at the end of the command or nots
    call forPrintCmd        ;Print cmd if in a FOR loop. Flag preserved.
    call setupRedirandPipes ;Setup/advance pipes and redir as appropriate
    call analyseCmdline     ;Setup cmdName and fcb for cmdBuffer portion
    call setDTA             ;Set the DTA back to us before we proceed!
    call doCommandLine      ;This preps and executes the command portion.
.okRet:                     ;Normal return point for processing
    call advanceRedir       ;Now advance and end redir if needed
    test byte [pipeFlag], -1    ;If no pipes, reset state, accept new input
    jz .inputMain
    ;Now we pull the commandline forwards. 
    call makeCmdString  ;Get offset into cmdBuffer + 2 of pipe in rsi
    lea rdi, cmdBuffer + 2
    mov rcx, rsi
    sub rcx, rdi    ;Get the number of chars to erase from cmd line 
    sub byte [cmdBuffer + 1], cl    ;And erase from the count
    mov cl, byte [cmdBuffer + 1]
    inc ecx     ;One more for the terminating char
    rep movsb   ;Move the chars over    
    call clearCommandState  ;Else, clear the command state and start again
    jmp short .pipeLoop     ;Doesn't close handles above 5 until end of pipe!

makeCmdBuffer:
;Makes the command buffer, escapes quotes and peels off any redirs from the
; copy buffer. Called only once in a cycle.
;Throughout: CL has char count, CH has quote count
    lea rsi, [cLineBuffer + 2]    ;Goto copy buffer + 2
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
    jz hardSynErr ;We had double pipe symbol, syntax error and reset!
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
    mov byte [cmdName], 0   ;Init this field to indicate no cmd
;Start by searching for any switch chars! Set byte if switch chars fnd!!
    lea rdi, qword [r8 + cmdLine]   ;Go to the command line in the psp
    mov rsi, rdi                    ;Prep rsi here too
    movzx ecx, byte [rdi - 1]       ;Get the count byte
    mov al, byte [switchChar]
    repne scasb                     ;Scan for the switch char
    jne .noSwitchFnd
    not byte [switchFnd]            ;Set switch char fnd on!
.noSwitchFnd:
    call skipDelimiters     ;Skip any preceeding separators from rsi
    cmp byte [rsi], CR      ;We have no command? Return!
    rete
    mov rbx, rsi            ;Save the start of the text in rbx
.plp:
    lea rdi, cmdFcb         ;Loop on the commandFCB
    mov eax, 2901h
    int 21h
    cmp al, -1  ;If DOS returns -1, we have a bad drive specified, fail!
    je .exitBadDrv
.trailingDotLp:
;Reverse along trailing dots!
    cmp byte [rsi - 1], "." ;Is this a dot?
    jne .noTrailingDots
    dec rsi
    jmp short .trailingDotLp
.noTrailingDots:
    mov al, byte [pathSep]  
    cmp byte [rsi], al      ;Is the terminator a pathsep?
    jne .notPs
    inc rsi ;Go to the char after the pathsep
    lodsb   ;Get this char
    dec rsi ;And move rsi back to where we were
    cmp al, 20h     ;Is this char below 32?
    jb .delimfnd    ;Also a delimiter
    cmp al, "\"     ;If this is a second pathsep, we are done
    je .delimfnd
    cmp al, "/"     ;Or an alt pathsep?
    je .delimfnd
    cmp al, byte "."
    je .delimfnd
    cmp al, byte '"'
    je .delimfnd
    call isALdelimiter  ;Is this a delim char?
    jne .plp            ;If not, we loop again
.delimfnd:
    dec rsi             ;Point rsi to end of the command
.notPs:
;Now we have reached the end of the command, rsi points to the first char
; after the command, whether a delimiter or not.
    mov rcx, rsi
    sub rcx, rbx    ;Get the number of chars in the command ONLY
    jz .exitBad     ;If this is zero, exit fail!
    xchg rbx, rsi   ;Swap the start and end of the commands!!!
    lea rdi, cmdPathSpec
    rep movsb
    xor al, al
    stosb   ;Store a terminating null
    xchg rbx, rsi
;Now we build FCBs for the arguments!
    lea rbx, qword [r8 + cmdLine]   ;Now we measure from the start of the buf!
    call .skipAndCheckCR
    je .setupCmdVars
    mov byte [arg1Flg], -1  ;Set that we are 
    mov rax, rsi
    sub rax, rbx            ;rbx points to the start of the buffer
    mov byte [arg1Off], al  ;Store the offset 
    lea rdi, qword [r8 + fcb1]
    mov eax, 2901h
    int 21h
    mov byte [arg1FCBret], al
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
    lea rdi, qword [r8 + fcb2]
    mov eax, 2901h
    int 21h
    mov byte [arg2FCBret], al
.setupCmdVars:
;Before returning, we copy the command name to cmdName 
    lea rdi, cmdPathSpec
    call findLastPathComponant  ;Point rdi to last path componant
    call strlen ;Get the length of the null terminated final path componant
    cmp byte [rdi + 1], ":"
    jne .noDrivePath
    mov al, byte [pathSep]
    cmp byte [rdi + 2], al
    je .noDrivePath
    ;Here if the filename is an FCB name A:12345678.9AB<NUL> (15 chars)
    cmp ecx, 15
    ja .exitBad
    jmp short .proceedCp
.noDrivePath:
;Normal paths come here
    cmp ecx, fileNameZL ;11 chars + ext sep + null terminator
    ja .exitBad     ;Return error
.proceedCp:
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
.skipAndCheckCR:
;Skips all chars, rsi points to the separator. If it is a CR, set ZF=ZE
    call skipDelimiters ;Go to the next char in the input line
    cmp byte [rsi], CR  ;If it is not a CR, it is an argument
    return
.exitBad:
    mov byte [cmdName], -1  ;Store -1 to indicate error
    return
.exitBadDrv:
    mov byte [cmdName], -2  ;Indicate a bad drive specified
    call badDriveError
    return

doCommandLine:
    ;The following check accounts for the end of a piped command
    cmp byte [cmdName], 0  ;If the cmd name length is 0, syntax error!
    je hardSynErr   ;This now should never be hit, earmark for removal!
    cmp byte [cmdName], -1  ;Error condition, command name too long!
    je badCmdError
    cmp byte [cmdName], -2  ;Bad drive specified, nop
    rete
    lea rsi, cmdPathSpec
    movzx ebx, word [cmdPathSpec]    ;Get the drive specifier
    cmp bh, ":"
    jne .noDriveSpecified
    xchg bl, al     ;Store drive status in bl, get letter in al
    call ucChar     ;Uppercase al
    sub al, "A"     ;And make it a 0 based drive letter
    ;If drive specified and cmdName length = 2 => X: type command
    cmp byte [cmdName], 2
    jne .noDriveSpecified   ;Drive specified but proceed as normal
    mov dl, al  ;Setdrive wants the number in dl
    call setDrive
    rete
    jmp badDriveError
.noDriveSpecified:
;rbx is writable UP TO THE FIRST PIPE OR CR (non-inclusive)
    lea rbx, cmdBuffer       ;Take your buffer
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
    ; as if it was an internal execution. rbx is not to be relied on here!
    lea rbx, qword [r8 + cmdTail]
    lea rsi, cmdName        
    mov eax, 0AE01h ;Execute command!
    mov edx, 0FFFFh
    mov ch, -1
    int 2Fh
    cmp byte [cmdName], 0 ;If this is non-zero, we restart the process
    retz    ;Return as normal if this is zero
    ;We need to copy over to cmdPathSpec in the event this command
    ; is an external command that is in the CD or in the PATH.
    ;Should not be used for this purpose but it is here...
    call pullCommandline    ;Pull the tail down with the original name
    lea rsi, cmdName        ;Now make the new cmd the new cmdspec!
    lodsb                   ;Get the name length
    movzx ecx, al
    lea rdi, cmdPathSpec    ;Overwrite the original specified command
    rep movsb   
    xor eax, eax
    stosb   ;Store null terminator
    jmp short .executeInternal2 ;Skip the equivalent for non-ae cases
.executeInternal:
    call pullCommandline    ;Now pull the tail down
    lea rdi, cmdPathSpec
    call strlen ;Get the length of the input command
    dec ecx     ;Minus 1 for terminating null
    cmp byte [cmdName], cl  ;Is it equal to the name of the command?
    jne launchChild     ;If not, a path was specified, exit!
.executeInternal2:
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
    ;Here it was found both strings are equal.
    lea rdi, qword [rbx + rcx + 1]  ;make rdi point to offset from startLbl
    movzx rbx, word [rdi]
    lea rdi, startLbl
    add rbx, rdi
    call rbx        ;Call the internal function!
;xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;No need for the below because the retcode is 
; zeroed when we it from DOS.
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;    mov byte [returnCode], 0 
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ;jmp short appRet    ;Now once we are done, goto appRet!
;xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

;This new exit preserves the retcode, and now allows for internal
; commands to implement retcodes which we don't use for now.
;I doubt we need to reset the stackptr as to get here, the stack has to
; have been balanced which means when we pop, we go back to okRet anyway...
    lea rsp, stackTop   ;Reset stack ptr! Unlikely needed!
    jmp commandMain.okRet   
.gotoNextEntry:
    add rbx, 3      ;Go past the first count byte and the address word
    add rbx, rcx    ;Go past the length of the command name too
    jmp short .nextEntry

appRet:  ;Return point from a task, jumped to from internal functions
;Can depend on RSP here because DOS stack bug fixed.
    lea rsp, stackTop   ;Reset stack ptr anyway.
;Start by resetting our PSP vectors (and IVT copies) in the event 
; they got mangled.
    call resetIDTentries
    call resetNation    ;Reset switchchar and nation if exernal cmd fired!
    mov eax, 4D00h              ;Get retcode, sets to 0 for internal commands
    int 21h
    mov word [returnCode], ax
    test ah, ah     ;Regular exit
    jz commandMain.okRet
    cmp ah, 3       ;TSR exit
    je commandMain.okRet
    ;Clean redirs and fully reset! This allows for safe returning if an error
    ; occured when invoked on Int 2Eh! Doesnt matter for normal single command
    ; as we always close the COMMAND.COM instance at this point.
    call cleanupRedirs
    jmp commandMain

hardSynErr:
;Hard syntax error in cmd line. Delete pipe files and reset completely!
    call badSyntaxError         ;Output bad syntax if empty command found
    jmp redirPipeFailureCommon.noPrint  ;This closes pipes and resets stack
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
.noPrint:
    call cleanupRedirs  ;Cleans the redirections 
    jmp commandMain ;Retake input from the command line

cleanupRedirs:
;Cleans all active redirections, returning to the saved state.
;Deletes any pipe files, leaves any redir out files.
;Resets the internal vars
    movzx eax, word [redirSTDIN]
    movzx edx, word [pipeSTDIN]
    xor ebx, ebx    ;Select STDIN for closing
    call .closeAndReplace
    movzx eax, word [redirSTDOUT]
    movzx edx, word [pipeSTDOUT]
    inc ebx         ;Select STDOUT for closing
    call .closeAndReplace
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
    return

.closeHandle:
    cmp ebx, -1
    rete
    mov eax, 3E00h
    int 21h
    return
.closeAndReplace:
;Input: ax = one possible handle, dx = second possible handle
;       bx = handle to close and copy ax/dx into
;Output: Handle intervened on. bx preserved
    shl eax, 10h    ;Shift low word into upper word
    or eax, edx     ;Form packed
    cmp eax, -1     ;If both -1, skip close!
    rete            ;Needed to ensure we dont fail silently
    call .closeHandle
    cmp ax, -1      
    cmove ax, dx    ;If ax is -1, move dx into ax
    push rbx
    movzx ebx, ax   ;Zero upper word of eax
    mov eax, 4500h  ;DUP this into the space formed by the close
    int 21h
    pop rbx
    retnc   ;If this succeeds, return
    ;Else we now try to force default device to open!
    call .closeHandle   ;Try close bx again!
    mov eax, 3D02h  ;Open read/write
    lea rdx, devName
    int 21h
    return

cleanRedirOut:
;Used to oust any stdout redir if necessary. This is different to the above in 
; that it deletes the redir out file. Therefore, this is only called in 
; particular cases like launching bat files which needs special handling in 
; that all redirs need to be completely removed.
    cmp word [redirSTDOUT], -1  ;Do we have > xyzzy.fil?
    rete  ;Return if no stdout redir! We are ok!
    movzx ebx, word [redirSTDOUT]
    mov ecx, 1      ;STDOUT
    mov eax, 4600h  ;DUP this into STDOUT closing redirout
    int 21h
    movzx ebx, word [redirSTDOUT]   ;Kill the duplicate now
    mov eax, 3E00h  
    int 21h
    mov word [redirSTDOUT], -1  ;Set default value back
    lea rdx, rdrOutFilespec
    mov eax, 4100h      ;Del stdout file
    int 21h
    mov byte [rdx], 0   ;Ensure this is a clear path (not necessary)
    ;Ignore any errors in this procedures. Errors mean either sharing problems
    ; or the file doesnt exist, which in either case, is fine to leave it be!
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
;       rsi points to the first char after the char in al in cLineBuffer
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
    call skipDelimiters ;Check if this is effectively a double || or |<CR>
    cmp byte [rsi], "|" 
    je .badFnd
    cmp byte [rsi], CR
.badFnd:
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
    test byte [redirOut], -1    ;Is redir out on?
    retz           
    cmp byte [pipeSTDOUT], -1   ;If pipe out is active, pause redirOut
    retne             ;Exit if it is
    ;Else setup the redir here for STDOUT
    mov ebx, 1    ;DUP STDOUT
    mov eax, 4500h
    int 21h
    jc redirFailure
    mov word [redirSTDOUT], ax   ;Save the handle in variable
    lea rdx, rdrOutFilespec
    cmp byte [redirOut], 1  ;If >, just create a new file!
    je .skipOpen
    mov eax, 3D02h  ;Open file for read write access
    int 21h
    jnc .fileExists
.skipOpen:
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
    lea rsi, qword [r8 + cmdLine]
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

getSetMainState:
;Resets the buffers lengths, sets stringops and gets the pPSP in r8
    cld ;Ensure stringops are done the right way
    mov byte [inBuffer], inLen      ;Reset the buffer length
    mov byte [cLineBuffer], inLen     ;Reset the buffer length
    mov byte [cmdBuffer], inLen     ;Reset the buffer length
    mov r8, qword [pPSP]              ;Reset the pPSP
    return


int2Eh: 
;Very sucky interface for passing command lines to be processed by the 
; current top level command interpreter. Will slowly try to patch to make it
; more reliable with a critical section flag. Currently, if we are already
; processing an Int 2Eh request, this will not process the command.
;
;Need to add further safeguards for when pipes are also setup and
; batchmode is on!
;
;Input: rsi points to the count byte of a command line.
;       Max number of chars is 128 (127 + count byte)
;Output:
;       CF=NC: Command was processed. ax = Retcode
;       CF=CY: Command was not processed.
    and byte [rsp + 2*8], ~1    ;Clear CF on entry
    test byte [statFlg1], inSingle
    jnz int2EcheckReentry 
.multiJoin:
    or byte [statFlg1], inSingle    ;Set the bits! Gets the lock!
    mov qword [int2Ersp], rsp       ;Save the far stack pointer 
    mov eax, 2F00h  ;Get the current DTA in rbx
    int 21h
    mov qword [int2Edta], rbx       ;We set the dta in the main loop later
    lea rsp, stackTop               ;Set to use the internal stack
    mov eax, 5100h  ;Get Current PSP in rdx
    int 21h
    mov qword [int2Epsp], rbx
    push rdx    ;Save on the stack
    mov rbx, qword [pPSP] ;Get the psp for this COMMAND.COM
    mov eax, 5000h ;Set this version of COMMAND.COM as the current PSP
    int 21h
    mov r8, rbx ;Set to point to the command.com psp
    mov r9, rbx
    cld
    lea rdi, cLineBuffer + 1
    mov ecx, cmdBufferL - 1
    rep movsb   ;Zoom zoom copy command line over
    call getSetMainState    ;Ensure the buffers have their lengths set
    call resetNation        ;Now ensure internationalisation is up to date
    cmp byte [cLineBuffer + 1], 0   ;Don't use batProceed to fail if len 0
    jne commandMain.goSingle    ;Proceed if we have anything to execute
int2ERet:
    call clearCommandLineState  ;Be a good citizen, leave it as we found it!
    mov rsp, qword [int2Ersp]
    mov rbx, qword [int2Epsp] ;Get Old current PSP in rbx
    mov eax, 5000h  ;Set Current PSP
    int 21h
    mov rdx, qword [int2Edta]
    mov eax, 1A00h  ;Set the DTA back to where it was upon return
    int 21h
    movzx eax, word [returnCode]    ;Get the return code in eax
    and byte [statFlg1], ~inSingle  ;Clear that we are in single mode
    iretq

int2EcheckReentry:
;Now we check if we DOSMGR is installed. If so, put task on ice
; else, return with CF=CY.
    mov eax, 5200h  ;Get sysvars
    int 21h
    test byte [rbx + 63h], -1   ;check the sysVars.dosMgrPresent byte
    jnz short .multifnd
    mov eax, 0300h   ;Are we here check on Int 2Fh
    int 2Fh
    test al, al ;This must still be zero, else something installed
    jnz .multifnd
    or byte [rsp + 2*8], 1  ;Else return with CF=CY, already processing
    iretq   
.multifnd:
;Recognised multitasker present, we now spinlock until flag is clear!
    pause
    test byte [statFlg1], inSingle   ;Is this bit set?
    jnz .multifnd
    jmp int2Eh.multiJoin            ;Rejoin the norm now