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
    jmp batNextLine         ;Now we start reading the batch file!

.bbCheckEndOfCmdLine:
;Input: rsi -> Char to check 
;Output: ZF=ZE if we hit a CR or a <NUL>
    cmp byte [rsi], 0
    rete
    cmp byte [rsi], CR
    return

batNextLine:
;This will:
;1) Open the batch file. If we are at the end of the file, exit batch mode!
;2) Read a line from the batch file one char at a time. File is open/closed
;       after each char. If file not found during read, print needBat error.
;       If file not found before read, print badBat error.
;       Do any %ENVVAR% or %ARGUMENT replacements
;       MAX LEN OF BATCH FILE LINE POST REPLACEMENT: 127 + CR or 128 chars raw
;3) Close the batch file
;4) Check if we are at the end of the file. If so, turn off bat flag.
    lea rdx, .l1
    mov eax, 0900h
    int 21h
    call batCleanup
    jmp commandMain
.l1 db "Batch mode... wait, what? How did you do that?",CR,LF,"$"

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
    