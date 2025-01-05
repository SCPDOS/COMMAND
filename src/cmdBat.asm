;Main Batch processing routines go here!

batLaunch:
;Start by creating the FQPath name and building a command line
; where the arguments are CR terminated.
;Then work out how much memory to allocate and allocate it.
;Start by saving the command line
    lea rsi, inBuffer
    lea rdi, batCmdline
    mov ecx, cmdBufferL
    rep movsb
;Now check if we are executing AUTOEXEC.BAT. If so, 
; we suppress F3 recalling of the command
    lea rsi, autoSpec + 3   ;Just check the name
    lea rdi, cmdFcb + fcb.filename
    mov ecx, 8  ;Only check filename as the extension here must be BAT
    repe cmpsb
    jne .notAutoexec
    mov byte [batCmdline + 1], 0    ;Set the count byte to 0
.notAutoexec:
    lea rsi, cmdPathSpec    ;Path here is null terminated.
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
    mov eax, 4700h  ;Get Current Directory (null terminated)
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
    call normalisePath  ;Normalise the path :)
;Now batFile has the FQpathname, construct the new CR delimited command line.
    lea rsi, cmdBuffer + 2
    lea rdi, cmdBuffer + 1  ;Overwrite count byte as we will null terminate
.copyCmdline:
    call skipDelimiters ;Find start of argument
.cclp:
    lodsb
    call isALdelimiter  ;If we hit delimiter, replace with CR, goto next arg
    jne .ccstore
    mov al, CR  
    stosb
    jmp short .copyCmdline
.ccstore:
    stosb   
    cmp al, CR  ;Did we just store a CR
    jne .cclp   ;Keep getting chars if so
    xor eax, eax    ;Else store a terminating null now
    stosb
    lea rdi, cmdBuffer + 1    ;Get back the ptr to the head of the new string
    call strlen     ;Get the new string length in ecx
    mov ebx, ecx    ;Save len in eax (include null)
    lea rdi, batFile
    call strlen     ;Get the filename len in ecx
    add ebx, ecx
    add ebx, batBlockHdr_size   ;Get the size to allocate for block
    mov ecx, ebx    ;Save the size in bytes in ecx for the cleaning below
    add ebx, 0Fh    ;Round up to nearest paragraph!
    shr ebx, 4      ;Convert to paragraphs
    mov eax, 4800h
    int 21h
    jnc .bbAlloced
    call badNoMemError  ;Print not enough mem error
    jmp  redirPipeFailureCommon.noPrint ;Clean up all redir and ret to cmdline
.bbAlloced:
;Now init the batblock with all the data we need
    mov qword [bbPtr], rax  ;Save the ptr here!
    mov rbx, rax
    mov rdi, rbx
    xor eax, eax
    rep stosb   ;Clean the block with nulls
    mov rdi, rbx            ;Point back to the head of the block
    mov al, byte [echoFlg]
    mov byte [rbx + batBlockHdr.bEchoFlg], al
    mov eax, -1
    mov ecx, 5
    lea rdi, qword [rbx + batBlockHdr.wArgs]    ;Init the wArgs to no params!
    rep stosd   ;Store in dwords for speed. Leave rdi pointing at .cmdLine    
    lea rsi, batFile
    call strcpy ;Copy the string and the terminating null
    lea rsi, cmdBuffer + 1
    push rdi    ;Save the ptr to where we will store the cmdline
    call strcpy ;Copy the command tail and the terminating null
    pop rdi     ;Get the pointer to the copied cmdline in rdi
    xor esi, esi    ;Use esi as argument counter
    xor ecx, ecx
    dec ecx ;Init ecx to large number for repne below (stupid hack will work)
.bbFndLp:
    cmp byte [rdi], 0   ;Is this the end of the cmdline?
    je .bbArgsDone      
;Else add the entry to the table! rbx -> batBlock
    mov rax, rdi
    sub rax, rbx    ;Now get distance from head of batBlock to this arg in ax
    mov word [rbx + 2*rsi + batBlockHdr.wArgs], ax    ;and store it!
    inc esi
    cmp esi, 10         ;Did we just process %9?
    je .bbArgsDone
    mov al, CR  ;Scan for the next CR and move rdi past it!
    repne scasb
    jmp short .bbFndLp   ;If not end of cmdline, see if next char delim
.bbArgsDone:
;Now deactivate any redirs. Do redir out as cleanupRedirs somewhat ignores it.
;Do the handle close as deleting the file without closing the handle is asking 
; for SHARING trouble...
    call cleanRedirOut      ;Liquidates redirout if needed
    call cleanupRedirs      ;Now liquidate remaining redirs and pipes
    or byte [statFlg1], inBatch ;Fire up the batch processor!
    jmp commandMain         ;Now we start reading the batch file!

batFinish:
;This is the procedure called after we've processed the last batch line
    call printPrompt    ;Add this to emulate what DOS does
    call batCleanup     ;Cleanup the batch and batch state vars etc etc
    jmp commandMain     ;And start again :)
batNextLine:
;Read the next line from the file and sets if we are done with copying
    test byte [statFlg1], batchEOF ;Did we hit EOF?
    jnz batFinish
    call batOpen    ;Open the batch file. Always succeeds. Hdl in ebx.
    mov rsi, qword [bbPtr]  ;Get the batch block ptr
    mov edx, dword [rsi + batBlockHdr.dBatOffLo]
    mov ecx, dword [rsi + batBlockHdr.dBatOffHi]
    mov eax, 4200h          ;LSEEK to where we left off previously
    int 21h
    mov byte [batInBuffer + 1], 0  ;Reset the buffer count
    lea rdx, batInBuffer + 2   ;Start read pos
    xor edi, edi            ;Use edi as the char counter
.readlp:
    call batReadChar        ;Read the char. Set ZF and flag if no bytes read.
    jz .eofAddCR
    inc rdi                 ;We read a char, woohoo!
    cmp byte [rdx], CR      ;End of line?
    je .eolCR
    cmp byte [rdx], LF      ;End of line UNIX?
    je .eolLF
    inc byte [batInBuffer + 1] ;Inc our char count
    inc rdx                 ;Store the next char in the next position
    cmp byte [batInBuffer + 1], inLen    ;Are we 128 chars w/o CR?
    jne .readlp             ;Get next char if not
    dec rdx                 ;Go back to the char we just read
    mov byte [rdx], CR      ;Overwrite with a terminating CR instead!!
    dec byte [batInBuffer + 1] ;Reduce the valid char count by one
    dec edi                 ;Ignore the 128th char that we read!
    jmp short .eol          ;The user typed too many chars on a line, EOL
.eofAddCR:
    mov byte [rdx], CR  ;Store a terminating CR on the line!
.eof:
    cmp byte [batInBuffer + 1], 0      ;If we read any chars, do the line!
    jne .eol
    call batClose                   ;Else close the hdl!
    jmp batFinish
.eolCR:   ;Now get the next char, to possibly eliminate a trailing LF
    call batReadChar  ;Get the LF over CR. Set ZF and flag if no bytes read.
    jz .eof     ;That CR was last char, check if we have something to do
    cmp byte [rdx], LF  ;Did we read a LF?
    jne .eolLF          ;Reread this char if not LF
    inc rdi             ;Else add to the count
.eolLF:
    mov byte [rdx], CR  ;Now place the CR over the last char
.eol:
;Close the file, update the batch block file pointer, then proceed.
;rsi -> Batch block.
    call batClose
;Imagine someone gives us a 2+Gb Batch file...
    add qword [rsi + batBlockHdr.qBatOff], rdi    ;Add to count
;Now we echo the prompt and command to the console unless the 
; first char is @, we hit a label or the echo flag is off.
    lea rdx, batInBuffer + 2
;Labels and @ chars are first non-delim char on line.
;Find the first non-delim char in the line and check it!!
    mov rsi, rdx
    call skipDelimiters     
    cmp byte [rsi], ":"     ;Label check! (:)
    je batNextLine          ;Just get the next line immediately
    call batPreprocess      ;Else we preprocess now

    lea rdx, batCpyBuffer + 2
    cmp byte [rdx], batNoEchoChar   ;Line no echo check! (@)
    je .noEchoPull       
    test byte [echoFlg], -1         
    jz commandMain.batProceed
    push rdx
    call printPrompt    ;Now output prompt
    pop rdx
    movzx ecx, byte [rdx - 1]    ;Get the number of chars to print
    mov ebx, 1  ;STDOUT
    mov eax, 4000h  ;Write woo!
    int 21h
    jmp commandMain.batProceedCrlf
.noEchoPull:
    dec byte [batCpyBuffer + 1]     ;Eliminate the @ char
    jz batNextLine    ;If this was just a @<CR><LF>, get next line
    mov rdi, rdx
    lea rsi, qword [rdx + 1]    ;Start from the char afterwards
    movzx ecx, byte [batCpyBuffer + 1]  ;Get the remaining count to copy
    inc ecx                         ;Want to copy over the terminating CR too
    rep movsb 
    jmp commandMain.batProceed   ;Now proceed normally w/o crlf

batPreprocess:
;Copies the line from batCpyBuffer to batInBuffer for regular processing,
; expanding any environment variables as the expansion takes place.
;Line is guaranteed only CR terminated.
    lea rbp, batCpyBuffer   ;Save the ptr for the expandVar function
    lea rdi, qword [rbp + 2]    ;Point to the string destination
    mov byte [rbp + 1], 0       ;Reset the buffer count
    lea rsi, qword [batInBuffer + 2]
    call skipDelimiters ;We strip leading delimiters
.lp:
    lodsb   ;Get char and advance source ptr
    cmp al, "%" ;Did we get a envvar symbol?
    jne .rawcp
    call batExpandVar   ;Advances rsi to next char and rdi past envvar
    jmp short .check  
.rawcp:
    stosb   ;Store the char
    cmp al, CR
    rete    ;Return immediately if we copied a CR. Dont add to count.
    inc byte [rbp + 1] ;Else inc the buffer count
.check:
    cmp byte [rbp + 1], inLen - 1   ;Max chars yet?
    jne .lp
    mov al, CR  ;Here if so, terminate the line nicely :)
    stosb   ;Store this char too
    return


batExpandVar:
;Expand any environment variables. 
;%% is treated as an escape char for a %.
;
;Input: rsi -> Char after the % sign that triggered this call.
;       rdi -> Position to place the substitution string.
;       rbp -> Head of destination buffer for copy.
;Output:
;   Substitution string is placed in buffer if necessary.
;       rsi -> Char after the terminal % of the source envvar name.
;       rdi -> Space for the next char to copy.
    movzx eax, byte [rsi]  ;Is this a parameter like %[0-9]?
    cmp al, "0"
    jb .notRep  ;If definitely not a number, keep going
    cmp al, "9"
    jb .repParm ;If a number, its a replacable parameter :)
.notRep:
    cmp al, CR          
    rete
    cmp al, "%" ;If immediately followed by %, then return it
    jne .notSecond
;Here we escape the second %. We cant enter here if we dont have space 
; at least 1 char, so store it immediately.
    stosb
    inc byte [rbp + 1]
    return
.notSecond:
;Now do the env var search. Start by scanning for the terminating
; % of the var name. If we strike a delimiter char first, 
; we stop the expansion for the envvar.
    mov rbx, rdi    ;Save where to write the envvar if one is found
    mov rdi, rsi    ;Maintain pointer to the head of the envvar string
.envVarLp:
    lodsb
    call isALdelimiter  ;Exit if a delimiter is hit first.
.lpExit:
    cmove rsi, rdi  ;If a delim found, return rsi to the char past the % sign.
    cmove rdi, rbx  ;And return rdi to where it was beforehand :)
    rete
    cmp al, CR      ;If we are at the end of the line too, exit!
    je .lpExit
    cmp al, "%"         ;Did we find a terminating % found.
    jne .envVarLp
;Fall here if we find the terminating % of the var name. rsi -> past %
    mov byte [rsi - 1], "=" ;Replace % with an equals sign for var search.
    push rsi    ;Save ptr to the first char past the envvar
    ;Take input rdi -> Varname to look for. Already in rdi.
    call searchForEnvVar    ;Returns rsi -> Envvar for copy in place
    cmovc rdi, rbx  ;Reset rdi here if no envvar found and exit!
    jc .exit
    mov rdi, rsi    ;Point rdi to the ASCIIZ envvar value itself
    mov al, "="
    mov ecx, -1
    repne scasb     ;Move rdi to the char past the equals sign
    pop rsi         ;Get back the ptr to the first char past the envvar
    retc

    push rsi        ;Save the ptr to the first char past the envvar
    mov rsi, rbx    ;Point rsi to where to copy the envvar
    call strlen     ;Get the string length of the envvar value in ecx
.copyVar:
    dec ecx         ;Drop 0 from count
    xchg rdi, rsi   ;Swap pointers for the copy
    movzx ebx, byte [rbp + 1] ;Get the count of chars already in the string
    push rcx
    add ecx, ebx    ;Get # of chars we will have.
    cmp ecx, inLen  ;If we end up with more than 127 chars, truncate
    pop rcx
    jb .noTrunc
    mov ecx, inLen - 1
    sub ecx, ebx    ;Turn into # of chars to copy, make space for CR
.noTrunc:
    add byte [rbp + 1], cl
    rep movsb   ;Moves rdi to the space for the next char
.exit:
    pop rsi     ;Get back the ptr to the first char past the envvar name
    return
.repParm:
    sub eax, "0"
    inc rsi ;Move the ptr past the replacable parameter value
    mov rbx, qword [bbPtr]
    movzx edx, word [rbx + batBlockHdr.wArgs + 2*rax]   ;Get off from cmdLine
    cmp edx, 0FFFFh   ;If there is no var, copy nothing and exit!
    rete
    push rsi        ;Save ptr to source of next chars 
    lea rsi, qword [rbx + rdx]  ;Save ptr to head of string to copy in rsi
    xor ecx, ecx
    dec ecx
    mov al, CR      ;Now scan for the terminating CR
    xchg rsi, rdi   ;Save dest ptr in rsi and point to string to cpy in rdi
    push rdi        ;Save the head of the string for copy
    repne scasb     ;Get the length of the string with terminating CR
    pop rdi         ;Point back to head of string
    not ecx         ;Convert to one less than the length (drops the CR)
    jmp short .copyVar

batCleanup:
;This function is called after the last line has been processed by the 
; batch interpreter! Cleans up all the batch resources. Also called if 
; CTRLC called during a batch job and the user wants to kill the batch.
    mov rbx, qword [bbPtr]
    test rbx, rbx
    jz .exit    ;Skip any references using this pointer
    mov al, byte [rbx + batBlockHdr.bEchoFlg]   ;Reset the echo flag
    mov byte [echoFlg], al
;-----------------------------------------------------------------------
;===Now free the FOR and CALL blocks... oops havent implemented yet!!===
; FOR blocks are generally cleaned up by the FOR command. CALL too. 
; But since this is the routine called by the error handler too, it 
; needs to check for these things. Not a big deal as normally we'll 
; just have a null pointer.
;-----------------------------------------------------------------------
    ;call forFree
;Finally free this batch header...
    push r8
    mov r8, rbx
    mov eax, 4900h
    int 21h
    pop r8
.exit:
    call cleanupRedirs  ;Clean up all redirections, close files etc
    mov qword [bbPtr], 0    
    and byte [statFlg1], ~(inBatch|batchEOF)   ;Oh bye bye batch mode!
;... and copy the batch command line back to its resting place.
    lea rsi, batCmdline
    lea rdi, inBuffer
    mov ecx, cmdBufferL
    rep movsb
    return

batOpen:
;Opens the batch file and returns the handle in ebx.
;Prints the "replace disk" string if file not found.
;
;Input: Nothing. Opens the filespec in the batFile.
;Output: ebx = File handle for filespec in batFile.
;
;Funky behaviour worth noting:
; If a failure occurs in open, we keep prompting the user to 
; replace the disk. The only way out if the error is really bad is 
; via ^C which does all the cleanup we need. 
; Thus this never returns fail.
    push rax
    push rdx
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
    pop rdx
    pop rax
    return

batClose:
;Close the handle in ebx.
    mov eax, 3E00h  ;Close the file pointer in ebx
    int 21h         
    return

batReadChar:
;Reads a char. 
;Input: ebx = Handle to read char from.
;       rdx -> Pointer to byte buffer to store byte.
;Output: 
;   CF=NC:
;       ZF=NZ: eax = 1. One char read.
;       ZF=ZE: eax = 0. EOF flag set in status byte. Zero or EOF char read.
;   CF=CY: Error in read. We act as if EOF reached. (Never checked.)
;Clobbers: None.
    push rcx
    mov ecx, 1
    mov eax, 3F00h
    int 21h 
    pop rcx 
    jc .bad     ;If CF, always act as if EOF. An error occured.
    test eax, eax   ;Here we check if we read 1 byte. (Clears CF)
    jz .eof         ;Jump to eofexit if we didn't read any bytes
    cmp byte [rdx], EOF ;Did we read ^Z char?
    retne           ;Return if the char we read was not an EOF char
.eof:
    pushfq      ;Preserve the flags for the bit toggle
    or byte [statFlg1], batchEOF    ;Set if we are done reading the file!
    popfq
    return    
.bad:
    xor eax, eax    ;Signal we hit an EOF (Set ZF)
    stc             ;Never check it but ensure reset of CF.
    jmp short .eof  ;And set the status bit
