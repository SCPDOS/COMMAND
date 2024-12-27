;Main Batch processing routines go here!

batLaunch:
;Preps and launches a batch file! Called with rdx pointing to the filespec :)
    mov ebx, bbMaxAlloc << 4    ;Convert to paragraphs
    mov eax, 4800h
    int 21h
    jnc .bbAlloced
    call badNoMemError  ;Print not enough mem error
    jmp  redirPipeFailureCommon.noPrint ;Clean up all redir and ret to cmdline
.bbAlloced:
    mov qword [bbPtr], rax  ;Save the ptr here!
    mov rbx, rax
    mov rdi, rbx
    xor eax, eax
    mov ecx, bbMaxAlloc     
    rep stosb               ;Clean the arena
    mov rdi, rbx            ;Point back to the head
    mov al, byte [echoFlg]
    mov byte [rbx + batBlockHdr.bEchoFlg], al
    mov eax, -1
    mov ecx, 5
    lea rdi, qword [rbx + batBlockHdr.wArgs]    ;Init the wArgs to no params!
    rep stosd   ;Store in dwords for speed. Leave rdi pointing at .cmdLine
    mov word [rbx + batBlockHdr.wArgs], 0   ;Arg %0 starts at offset 0!
    lea rsi, cmdPathSpec
    call strcpy     ;Leave rdi past the terminating null
    movzx ecx, byte [r8 + cmdLineCnt]  ;Get char cnt for copy
    lea rsi, qword [r8 + cmdLine]   ;Get copy source
    push rdi    ;Save the ptr to the start of cmd tail in batblock
    rep movsb   ;Copy the command tail over
    ;Since this copy is shorter than the space we have, we already have a free
    ; terminating null. All good!
    pop rsi 
    ;Now analyse the command line to get the word offsets. Get at most 10
    mov ecx, 1  ;Start with argument 1
.bbFndLp:
    call skipDelimiters ;Skip leading delimiters, leave rsi at char1
    call .bbCheckEndOfCmdLine   ;Is this the end of the command?
    je .bbArgsDone      ;Yes
    ;Add the entry to the table!
    mov rax, rsi
    lea rdx, qword [rbx + batBlockHdr.cmdLine]  ;Get addr of start of cmdline
    sub rax, rdx    ;Now get the difference in ax
    mov word [rbx + batBlockHdr.wArgs + rcx], ax    ;Store this offset here
    
    inc ecx
    cmp ecx, 10         ;Did we just process %9?
    je .bbArgsDone
.bbFndLp2:
    lodsb   ;Getch
    call isALdelimiter  ;If this is a delimiter, we are at the end of the command
    je .bbFndLp
    call .bbCheckEndOfCmdLine
    jne .bbFndLp2   ;If not end of cmdline, see if next char delim
.bbArgsDone:
;Now copy the batch name, need to figure the full path to it.
    lea rsi, cmdPathSpec
    lea rdi, batFile
    mov ax, word [rsi]  ;Get the first two chars
    cmp ah, ":"
    je .bbdrvGiven
    cmp al, byte [pathSep]  ;Is the first char a pathsep?
    je .bbCDrvAbs
;Current drive relative
    call getCurrentDrive    ;Gets the 0 based current drive in al
    add al, "A"
    mov ah, ":"
    stosw   ;Store these two chars, adv rdi
.bbRelPath:
    mov al, byte [pathSep]  
    stosb   ;Store the pathsep
    mov al, byte [batFile]  ;Now get the drive letter into al
    call ucChar             ;UC it
    mov byte [batFile], al  ;and overwrite it :)
    sub al, "@"             ;Convert into a 1 based drive number
    mov dl, al
    push rsi        ;Save remaining char source
    mov rsi, rdi    ;rdi is where we want to store the file name
    mov eax, 4700h  ;Get Current Directory
    int 21h
    pop rsi
    jnc .bbRelPathOk
    lea rdx, badBat
    call printString
    call batCleanup     ;Now clean up the batch stuff we've setup
    jmp commandMain     ;And start again :)   
.bbRelPathOk:
;Now move rdi to the terminating null   
    xor eax, eax
    xor ecx, ecx
    repne scasb ;Find the terminating null
    dec rdi ;Now point to the terminating null
    mov al, byte [pathSep]
    stosb   ;Store this pathsep over the original null
    ;Now we are ready to copy the command line passed to us by the user
    ; to rdi. rsi points to where to source the rest of the chars
    jmp short .bbCpName
.bbCDrvAbs:
;Current drive absolute. Get current drive into buffer
    call getCurrentDrive    ;Gets the 0 based current drive in al
    add al, "A"
    mov ah, ":"
    stosw   ;Store these two chars
    ;Now we are ready to copy the command line passed to us by the user
    ; to rdi. rsi points to where to source the rest of the chars
    jmp short .bbCpName
.bbdrvGiven:
;Drive given X:
    movsw   ;Move over the X:, point rsi to the first new char
    lodsb
    dec rsi ;Get the char and point back to it
    cmp al, byte [pathSep]  ;Is char three a pathsep?
    jne .bbRelPath
.bbCpName:
    call strcpy ;Copy the remaining portion
    lea rsi, batFile
    mov rdi, rsi
    mov eax, 1211h  ;Normalise the path :)
    int 2fh
;Now deactivate any redirs. Do redir out as cleanupRedirs somewhat ignores it.
;Do the handle close as deleting the file without closing the handle is asking 
; for SHARING trouble...
    call cleanRedirOut      ;Liquidates redirout if needed
    call cleanupRedirs      ;Now liquidate remaining redirs and pipes
    or byte [statFlg1], inBatch ;Fire up the batch processor!
    jmp commandMain         ;Now we start reading the batch file!

.bbCheckEndOfCmdLine:
;Input: rsi -> Char to check 
;Output: ZF=ZE if we hit a CR or a <NUL>
    cmp byte [rsi], 0
    rete
    cmp byte [rsi], CR
    return

batFinish:
;This is the procedure called after we've processed the last batch line
    call printPrompt    ;Add this to emulate what DOS does
    call batCleanup     ;Cleanup the batch and batch state vars etc etc
    jmp commandMain     ;And start again :)
batNextLine:
;Read the next line from the file and sets if we are done with copying
    test byte [statFlg1], batchEOF ;Did we hit EOF?
    jnz batFinish
    lea rdx, batFile
.batOpen:
    mov eax, 3D00h  ;Open for read only
    int 21h
    jnc .batOpened
    lea rdx, needBat
    call printString
    mov eax, 0800h  ;CON input w/o echo. Allows for triggering ^C
    int 21h
    jmp short .batOpen
.batOpened:
    mov ebx, eax            ;Move the handle into ebx
    mov rsi, qword [bbPtr]  ;Get the batch block ptr
    mov edx, dword [rsi + batBlockHdr.dBatOffLo]
    mov ecx, dword [rsi + batBlockHdr.dBatOffHi]
    mov eax, 4200h          ;LSEEK to where we left off previously
    int 21h
    mov byte [inBuffer + 1], 0  ;Reset the buffer count
    lea rdx, inBuffer + 2   ;Start read pos
    xor edi, edi            ;Use edi as the char counter
.readlp:
    call .readChar          ;Read the char
    test eax, eax
    jz .endOfBatAddCr
    inc edi                 ;We read a char, woohoo!
    cmp byte [rdx], EOF     ;Did we read a ^Z char?
    je .endOfBatAddCr
    cmp byte [rdx], CR      ;End of line?
    je .endOfLineCr
    cmp byte [rdx], LF      ;End of line UNIX?
    je .endOfLineLf
    inc byte [inBuffer + 1] ;Inc our char count
    inc rdx                 ;Store the next char in the next position
    cmp byte [inBuffer + 1], inLen    ;Are we 128 chars w/o CR?
    jne .readlp             ;Get next char if not
    dec rdx                 ;Go back to the char we just read
    mov byte [rdx], CR      ;Overwrite with a terminating CR instead!!
    dec byte [inBuffer + 1] ;Reduce the valid char count by one
    dec edi                 ;Ignore the 128th char that we read!
    jmp short .endOfLine    ;The user typed too many chars on a line, EOL
.endOfBatAddCr:
    mov byte [rdx], CR  ;Store a terminating CR on the line!
.endOfBat:
    or byte [statFlg1], batchEOF    ;Set if we encounter a ^Z terminator
    cmp byte [inBuffer + 1], 0      ;If we formally read 0 chars, exit!
    jne .endOfLine
    call .closeBat                  ;Close the hdl! This is why this is here!
    jmp batFinish
.endOfLineCr:   ;Now get the next char, to possibly eliminate a trailing LF
    call .readChar  ;Get the LF over the CR
    test eax, eax   ;Did we read nothing?
    jz .endOfBat    ;That CR was last char, check if empty buffer, else exec
    cmp byte [rdx], LF  ;Did we read a LF?
    jne .endOfLineLf    ;Reread this char if not LF
    inc edi             ;Else add to the count
.endOfLineLf:
    mov byte [rdx], CR  ;Now place the CR over the last char
.endOfLine:
;Close the file, update the batch block file pointer, then proceed.
;rsi -> Batch block.
    call .closeBat
    ;Imagine someone gives us a 2+Gb Batch file... some server magik lmao
    add dword [rsi + batBlockHdr.dBatOffLo], edi    ;Add lo dword to chars 
    adc dword [rsi + batBlockHdr.dBatOffHi], 0      ;Add CF if needed!
;Now we echo the line to the console unless the first char is @ or 
; the echo flag is off
    lea rdx, inBuffer + 2
    cmp byte [rdx], batNoEchoChar
    je .noEchoPull       
    test byte [echoFlg], -1         
    jz commandMain.batProceed
    push rdx
    call printPrompt    ;Now output prompt
    pop rdx
    movzx ecx, byte [inBuffer + 1]    ;Get the number of chars to print
    mov ebx, 1  ;STDOUT
    mov eax, 4000h  ;Write woo!
    int 21h
    jmp commandMain.batProceedCrlf
.noEchoPull:
    dec byte [inBuffer + 1]     ;Eliminate the @ char
    jz batNextLine    ;If this was just a @<CR><LF>, get next line
    mov rdi, rdx
    lea rsi, qword [rdx + 1]    ;Start from the char afterwards
    movzx ecx, byte [inBuffer + 1]  ;Get the remaining count to copy
    inc ecx                         ;Want to copy over the terminating CR too
    rep movsb 
    jmp commandMain.batProceed   ;Now proceed normally w/o crlf
    
.closeBat:
;Close the handle in rbx
    mov eax, 3E00h  ;Close the file pointer in ebx
    int 21h         ;We ignore errors here... dont hurt me SHARE pls
    return
.readChar:
;Reads a char. If no chars read, sets the EOF flag!
    mov ecx, 1
    mov eax, 3F00h
    int 21h  
    test eax, eax
    retnz   ;If a char read, return
    or byte [statFlg1], batchEOF    ;Set the end of file reached flag!
    return


batExpandVar:
;Input: rsi -> Char after the % sign that triggered this call.
;       rdi -> Position to place the substitution string
;Output: CF=NC: Substitution string is placed in buffer
;        CF=CY: No substitution string found
    return
    ;cmp byte [rsi], "%"

batCleanup:
;This function is called after the last line has been processed by the 
; batch interpreter! Cleans up all the batch resources. Also called if 
; CTRLC called during a batch job and the user wants to kill the batch.
    mov rbx, qword [bbPtr]
    mov al, byte [rbx + batBlockHdr.bEchoFlg]   ;Reset the echo flag
    mov byte [echoFlg], al
;-----------------------------------------------------------------------
;===Now free the FOR and CALL blocks... oops havent implemented yet!!===
; FOR blocks are generally cleaned up by the FOR command. CALL too. 
; But since this is the routine called by the error handler too, it 
; needs to check for these things. Not a big deal as normally we'll 
; just have a null pointer.
;-----------------------------------------------------------------------
;Finally free this batch header
    push r8
    mov r8, rbx
    mov eax, 4900h
    int 21h
    pop r8
    call cleanupRedirs  ;Clean up all redirections, close files etc
    mov qword [bbPtr], 0    
    and byte [statFlg1], ~(inBatch|batchEOF)   ;Oh bye bye batch mode!
    return