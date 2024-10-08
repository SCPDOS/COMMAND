critErrorHandler:   ;Int 24h
;User Stack in usage here, must be swapped to before this is called
;Entered with:  
;               AH = Critical Error Bitfield
;               Bit 7 = 0 - Disk Error, Bit 7 = 1 - Char Device Error
;               Bit 6 - Reserved
;               Bit 5 = 0 - IGNORE not allowed, Bit 5 = 1 - IGNORE allowed
;               Bit 4 = 0 - RETRY not allowed, Bit 4 = 1 - RETRY allowed
;               Bit 3 = 0 - FAIL not allowed, Bit 3 = 1 - FAIL allowed
;               Bits [2-1] = Affected Disk Error
;                     0 0   DOS area
;                     0 1   FAT area
;                     1 0   Directory area
;                     1 1   Data area
;               Bit 0 = 0 - Read Operation, Bit 0 = 1 - Write Operation
;               AL  = Failing drive number if AH[7] = 0
;               DIL = Error code for errorMsg
;               RSI = EA of Device Header for which device the error occured
;Return:
;               AL = 0 - Ignore the Error       (Ignore)
;                  = 1 - Retry the Operation    (Retry)
;                  = 2 - Terminate the Program  (Abort)
;                  = 3 - Fail the DOS call      (Fail)
    push rbx
    push rcx
    push rdx
    push rdi
    push rsi
    call errSwapHdls
    cld         ;Make String ops go forward
    mov bx, ax  ;Save ah in bh and al in bl (if needed)
    mov byte [failDrv], al  ;Setup the failing drive (even on char as we reset)
    lea rdx, crlf
    call printString    ;Trashes ax
    movzx edi, di                   ;Clear the upper word.

    push rbx        ;Save the action bitfield
    push rsi        ;Save the driver pointer
    mov eax, 5900h  ;Get Extended Error
    int 21h
;DOS placed the following values in the following regs:
;ax = word [errorExCde]
;ch = byte [errorLocus]
;bh = byte [errorClass]
;bl = byte [errorAction]
;rdi = qword [errorVolLbl]
    lea rsi, errorMsgTbl.FVol
    xchg rdi, rsi   ;Swap the pointers
    movsq   ;Move over the 11 chars :)
    movsw
    movsb
    movzx edi, ax  ;Move the error code into di
    pop rsi
    pop rbx
    cmp edi, errGF      ;Is this a normal driver error?
    jbe .driverErr
;Now we split the driver from sharing and networking errors. Sharing doesnt go 
; thru the redir
    cmp edi, errShrFul
    jbe .shareErr
;Ok so this is a net error. Check to see if an installed message!
;di has the error code still
    mov eax, 0500h      ;Install check!
    int 21h
    cmp al, -1
    jne .redirDefault   ;No redir, print default net error
    mov eax, edi        ;Else, move the error code into 
    mov ah, 05h         ;Get the string we need
    int 21h
    jc .redirDefault    ;If no message installed for this code, generic!
    ;Returned if CF=NC:
    ; al = 0 => Print rest of message
    ; al = 1 => Immediately prompt ARIF
    ; rdi -> ASCIIZ string to print
    mov rdx, rdi        
    push rax            
    xor eax, eax
    mov ecx, eax
    dec ecx
    repne scasb         ;Search for the terminating null
    mov byte [rdi - 1], "$" ;Replace with dos string terminator
    call printString 
    mov byte [rdi - 1], 0   ;Replace with sane string terminator
    pop rax
    test al, al ;Is this zero?
    jz .proceedNormalWrite  ;Now print reading/writing etc
    jmp .userInput    ;Else, print crlf and proceed to get input
.redirDefault:
;Always jumped to with rdi in the error code. Thus, this will print only
; this line with no reading/writing etc. rdi is above errShrFul here
; and also not a table offset so definitely wont accidentally try print
; additional information
    lea rdx, genNetErr  ;Set the generic network error message
    jmp short .redirDefProceed
.shareErr:
;Now ensure our error code is in the table, set to GF error if not.
    mov edx, errGF
    cmp edi, errShrFul
    cmova edi, edx
.driverErr:
    mov edx, errGF      ;If we have an error below Driver Error 0, Gen. Err.
    cmp edi, drvErrShft
    cmovb edi, edx
    sub edi, drvErrShft ;Now reduce the error code to be a table offset
    push rdi    ;Save the error code for checking
    lea rdx, errMsgPtrTbl
    xchg rdi, rdx   ;Swap error code and table base
    movzx edx, word [rdi + 2*rdx]   ;Get the word offset in rdx
    add rdx, rdi            ;Now add the table base!
    pop rdi
.redirDefProceed:
    call printString        ;Call DOS to print first part of message
    ;Now we handle any codes above errNoFCB - drvErrShft as 
    ; to not print anything other than the string in the table!
    ;cmp edi, errNoFCB - drvErrShft
    cmp edi, errIDC - drvErrShft
    jae .userInput  ;IDC also triggers this skip
.proceedNormalWrite:
    lea rdx, readMsg
    lea rdi, writeMsg
    test bh, 1  ;Bit 0 is set if write operation
    cmovnz rdx, rdi ;Move the correct r/w part of the message to rdx
    call printString     ;Call DOS to print error reading/writing portion

    test bh, 80h    ;Test bit 7 for char/Disk assertation
    jnz .charError
;Disk error continues here
    lea rdx, drvMsg ;Drive message
    call printString
    mov dl, bl  ;Get zero based drive number into dl
    add dl, "A" ;Add ASCII code
    mov ah, 02h ;Print char in dl
    int 21h
    
    mov eax, ebx    ;Get bh into ah
    and eax, 600h   ;Mask off bits 1 and 2 of bh
    cmp eax, 200h   ;Was this a FAT buffer?
    jne .userInput  ;If not proceed as normal.
    ;Else, abort! Application cannot proceed if FAT is breaking apart...
    mov al, 2   ;Abort! Lose that FAT buffer
    jmp .cehExit
.userInput:
    call printCRLF  ;Print new line
;Abort, Retry, Ignore, Fail is word order
;Last message gets a ?, otherwise a comma followed by a 20h (space)
.userAbort:
;Abort is always an option
    lea rdx, abortMsg
    call printString ;Call DOS to prompt user for ABORT option
.userRetry:
    test bh, 10h  ;Bit 4 is retry bit
    jz .userIgnore    ;If clear, dont print message
    lea rdx, betweenMsg
    call printString
    lea rdx, retryMsg
    call printString
.userIgnore:
    test bh, 20h    ;Bit 5 is ignore bit
    jz .userFail
    lea rdx, betweenMsg
    call printString
    lea rdx, ignoreMsg
    call printString
.userFail:
    test bh, 08h    ;Bit 3 is Fail bit
    jz .userMsgEnd
    lea rdx, betweenMsg
    call printString
    lea rdx, failMsg
    call printString
.userMsgEnd:
    lea rdx, endMsg
    call printString
;Get user input now 
    xor ecx, ecx  ;4 Possible Responses
    lea rdi, i24Resp ;Go to start of string
    mov eax, 0C01h ;Flush and get STDIN without Console Echo
    int 21h ;Get char in al
    cmp al, "a" ;Chack if lowercase, consider using UC char DOS multiplex
    jb .uip1    ;If the value is below, ignore subtraction
    sub al, "a"-"A"  ;Turn the char into uppercase
.uip1:
    scasb   ;Compare char to list, offset gives return code
    je .validInput  ;If they are equal, ecx has return code
    inc ecx
    cmp ecx, 4
    jne .uip1
    jmp .userInput ;If valid char not found, keep waiting 
.validInput:
    call printCRLF   ;Note the input was accepted
    mov al, cl  ;Move the offset into .responses into al
;Now check if the input is permitted
    cmp al, 2   ;Check if abort, abort always permitted
    je .cehExit
    test al, al ;Check if 0 => Ignore
    je .viIgnore
    cmp al, 1   ;Check if 1 => Retry
    je .viRetry
.viFail:    ;Fallthrough for fail (al = 3)
    test bh, 8  ;Bit 3 is Fail bit
    jz .userInput  ;If bit 3 is zero, prompt and get input again
    jmp short .cehExit
.viIgnore:
    test bh, 20h    ;Bit 5 is Ignore bit
    jz .userInput
    jmp short .cehExit
.viRetry:
    test bh, 10h    ;Bit 4 is Retry bit
    jz .userInput
.cehExit:
    call errRetHdls
    pop rsi
    pop rdi
    pop rdx
    pop rcx
    pop rbx
    iretq
.charError:
    mov byte [failDrv], -1  ;Clear the failing drive 
    mov ecx, 8  ;8 chars in device name
    add rsi, drvHdr.drvNam  ;Get the address of the Drive name
.ce1:
    lodsb   ;Get a string char into al and inc rsi
    mov dl, al  ;Move char into dl
    mov ah, 02h
    int 21h ;Print char
    loop .ce1   ;Keep looping until all 8 char device chars have been printed
    jmp .userInput


errSwapHdls:
;Swaps STDIO to STDERR
;Start by tmporarily moving the stderr handler into stdio
;In principle dangerous, but since we cannot exit our routine, its oki.
    push rax
    push rbx
    call getJftPtr  ;Get the jft pointer into rbx. Saves rax
    movzx eax, word [rbx]       ;Get STDIO into ax
    mov word [errHdls], ax      ;Save em
    movzx eax, byte [rbx + 2]   ;Now get the STDERR SFTndx in al
    mov ah, al                  ;Move STDERR hdl into ah too 
    mov word [rbx], ax          ;And set STDIO to STDERR
    pop rbx
    pop rax
    return

errRetHdls:
;Returns STDIO from STDERR.
    push rax
    push rbx
    call getJftPtr  ;Get ptr in rbx. Preserves rax.
    movzx eax, word [errHdls]
    mov word [errHdls], -1  ;Reset values
    mov word [rbx], ax      ;Store the handle word back
    pop rbx
    pop rax
    return

getJftPtr:
;Preserves all registers except rbx.
;Output: rbx -> JFT of current task
    push rax        ;Save rax
    mov eax, 5100h  ;Get current PSP in rbx
    int 21h
    pop rax
    cmp word [rbx + psp.jftSize], 20    ;If >20, pspjft is ptr to real jft
    jbe .pspJftExit
    mov rbx, qword [rbx + psp.externalJFTPtr]   ;Get ptr to jft from the psp
    return
.pspJftExit:
    lea rbx, qword [rbx + psp.jobFileTbl]       ;Make into a ptr to jft in psp
    return
