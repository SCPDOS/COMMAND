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
    lea rdx, crlf
    call printString

    and edi, 00FFh   ;Zero the upper bytes of DI just in case
;    cmp edi, 0Fh     ;Is this special case error 15h?
;    jne .notError15
; Need to do Extended Error call to get the ptr to the volume label.
; Disk driver doesnt currently update the volume label in the BPB and doesn't
; place the volume label in the field in the driver block, but DOS assumes it 
; does. Once that is implemented, I will activate this section of code!
.notError15:
    mov ecx, 0Ch
    cmp edi, ecx  ;Check if the error number is erroniously above Gen Error
    cmova edi, ecx  ;If it is, move Gen Error into edi
    movzx rdi, di
    mov rdx, rdi    ;Copy error code
    shl rdi, 4  ;Multiply by 16
    shl rdx, 1  ;Multiply by 2
    add rdi, rdx    ;Add the resultant multiplications
    lea rdx, errorMsgTable
    lea rdx, qword [rdx+rdi]   ;Load EA to rdx
    call printString     ;Call DOS to print first part of message

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

int23h:
    test byte [statFlg1], inCtrlC
    jnz .inInt23    ;IRETQ immediately if we are in int23
    or byte [statFlg1], inCtrlC  ;Set that we are in CTRL+C
    test byte [statFlg1], inBatch ;Are we processing a batch file?
    jz .notBat
    call errSwapHdls    ;Swap STDIO back
.ynLp:
    lea rdx, batFail
    call printString
    lea rdx, batYNstr
    mov eax, 0A00h      ;Get buffered input!
    int 21h
    mov al, byte [batYNstr + 3]
    call ucChar         ;UC using DOS
    cmp al, "Y"
    je .killBat
    cmp al, "N"
    jne .ynLp
    and byte [statFlg1], ~inCtrlC   ;Exiting, now safe to reenter!
.inInt23:
    iretq   ;Ignore the CTRLC
.killBat:
    call errRetHdls
    jmp short .exit
.notBat:
    push rax        ;Save rax, ah contains call we are in that ^C'ed
    mov eax, 5100h  ;Get current PSP in rbx
    int 21h
    pop rax
    cmp rbx, qword [pspPtr] ;Was the task us?
    jne .exit       ;If not, then immediately abort it!
    test byte [permaSwitch], -1 ;If not permanent, skip parent PSP swap
    jnz .noJug   ;Avoid setting "real parent"
    ;Else, we juggle parent PSP's
    push rax
    push rbx
    mov rax, qword [realParent]
    mov rbx, qword [pspPtr]
    mov qword [rbx + psp.parentPtr], rax    ;Store the parent there
    pop rbx
    pop rax
.noJug:
    cmp word [errHdls], -1  ;If these are not -1, return to normal!
    je .exit
    call errRetHdls 
.exit:
    and byte [statFlg1], ~inCtrlC
    stc     ;Set CF to kill the task
    ret 8   ;Return and pop CS off the stack to indicate we wanna kill task