;Main Batch processing routines go here!

batLaunch:
;Preps and launches a batch file!
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
    mov ecx, bbMaxAlloc     
    rep stosb               ;Clean the arena
    mov rdi, rbx            ;Point back to the head
    mov al, byte [echoFlg]
    mov byte [rbx + batBlockHdr.bEchoFlg], al
    mov qword [rbx + batBlockHdr.dBatOffLo], 0 ;Write a qword of 0 for zoom
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
    ;Now copy the batch name
    lea rsi, cmdPathSpec
    lea rdi, batFile
    call strcpy             ;Copy the batch file name over
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
    call batCleanup     ;Cleanup the batch and batch state vars etc etc
    jmp commandMain     ;And start again :)
batNextLine:
;This will:
;1) Open the batch file. If we are at the end of the file, exit batch mode!
;2) Read a line from the batch file one char at a time. File is open/closed
;       after each char. If file not found during read, print needBat error.
;       If file not found before read, print badBat error.
;       Do any %ENVVAR% or %ARGUMENT replacements
;       MAX LEN OF BATCH FILE LINE: 127 + CR or 128 chars raw
;3) Close the batch file
;4) Check if we are at the end of the file. If so, turn off bat flag.
;    lea rdx, .l1
;    mov eax, 0900h
;    int 21h
;    call batCleanup
;    jmp commandMain
;.l1 db "Batch mode... wait, what? How did you do that?",CR,LF,"$"
    test byte [statFlg1], batchEOF ;Did we hit EOF?
    jnz batFinish
    lea rdx, batFile
    mov eax, 3D00h  ;Open exclusively
    int 21h
    jnc .batOpened
    ;!!! BAT FILE OPEN ERROR HANDLING HERE !!!
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
    jz .endOfBat
    inc edi                 ;We read a char, woohoo!
    cmp byte [rdx], EOF     ;Did we read a ^Z char?
    je .endOfBat
    cmp byte [rdx], CR      ;End of line?
    je .endOfLineCr
    inc byte [inBuffer + 1] ;Inc our char count
    inc rdx                 ;Store the next char in the next position
    cmp byte [inBuffer + 1], 128    ;Are we 128 chars w/o CR?
    jne .readlp             ;Get next char if not
    jmp short .endOfLine    ;The user typed too many chars on a line, EOL
.endOfBat:
    cmp byte [inBuffer + 1], 0  ;If we formally read 0 chars, exit immediately
    je batFinish
    jmp short .endOfLine
.endOfLineCr:   ;Now get the next char, to eliminate the LF too.
;Properly, I should check if this is LF or not. If not an LF, we move the 
; file pointer back a char. 
    breakpoint
    call .readChar  ;Get the LF over the CR
    mov byte [rdx], CR  ;Place the CR back 
.endOfLine:
;Close the file, update the batch block file pointer, then proceed.
;rsi -> Batch block.
    mov eax, 3E00h  ;Close the file pointer in ebx
    int 21h         ;We ignore errors here... dont hurt me SHARE pls
    ;Imagine someone gives us a 2+Gb Batch file... some server magik
    add dword [rsi + batBlockHdr.dBatOffLo], edi    ;Add lo dword to chars 
    adc dword [rsi + batBlockHdr.dBatOffHi], 0      ;Add CF if needed!
;Now we echo the line to the console unless the first char is @ or 
; the echo flag is off
    lea rdx, inBuffer + 2
    cmp byte [rdx], batNoEchoChar
    je .noEcho       
    test byte [echoFlg], -1         
    jz .noEcho
    movzx ecx, byte [inBuffer + 1]    ;Get the number of chars to print
    mov ebx, 1  ;STDOUT
    mov eax, 4000h  ;Write woo!
    int 21h
.noEcho:
    jmp commandMain.batProceed
.readChar:
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
    and byte [statFlg1], ~inBatch   ;Oh bye bye batch mode!
    return