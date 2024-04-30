int23h:
    test byte [statFlg1], inLdr     ;Are we loading?
    jz .notLoading
    test byte [statFlg1], inLdrDT   ;Are we in date/time?
    jz  .inInt23                    ;IRETQ if not.
    ;Else, we pretend nothing was typed in and proceed.
    mov word [rdx + 1], 0D00h       ;Place a 0 chars cnt and CR in the buffer
    jmp short .retFromDosCall       ;And return directly to DOS
.notLoading:
    test byte [statFlg1], inCtrlC   ;In this handler already?
    jz .notNested                   ;If not, handle!
;We are in this CTRL+C, check the system call that we are interrupting...
;If a CON call, we are in Terminate Batch Job msg. Set CF to indicate
; to US that the user CTRL+C'd the Terminate Batch Job message! Return
; from DOS directly, do not reenter CON call.
    test ah, ah
    jz .inInt23 
    cmp ah, 0Ch
    ja .inInt23
.retFromDosCall:
;Go past the Int 23h stack frame to the entry to Int 21h stack frame
    push rax
    lea rax, .i23Bounce
    mov qword [rsp + 8], rax        ;Store as ret add
    xor eax, eax
    mov ax, cs
    mov qword [rsp + 2*8], rax      ;Store the segment too
    pop rax
    iretq   ;Iretq to the next instruction instead of DOS :)
.i23Bounce:
    or byte [rsp + 2*8], 1  ;Set CF on stack frame flags!
.inInt23:
    iretq
.notNested:
;Main body of the CRTL+C handler! 
    or byte [statFlg1], inCtrlC     ;Set that we are in CTRL+C
    test byte [statFlg1], inSingle  ;Are we in single command mode?
    jnz .skipReset                  
    ;Now reset the disk subsystem to flush buffers appropriately,
    ; in case of open files and/or we are hooked by someone who doesn't
    ; want to return thru dos.
    push rax
    mov eax, 0D00h  ;Reset Disk system!
    int 21h
    pop rax
.skipReset:
    test byte [statFlg1], inBatch   ;Are we processing a batch file?
    jz .notBat
    test byte [statFlg1], inSingle  ;Batch in a single command?
    jnz .notBat                     ;No batch cleanup, kill ourselves!
;CTRLC in a batch file will always abort the current command being 
; executed. CTRLC in a batch file is only for checking if we want to 
; terminate the whole batch procedure or not!
    call errSwapHdls    ;Swap STDIO back ONLY ON BATCH Error
.ynLp:
    lea rdx, batFail
    call printString
    lea rdx, batYNstr
    mov eax, 0C0Ah      ;Get clean buffered input!
    int 21h
    jc .killBat         ;If we CTRL+C during this call, kill batch!
    mov al, byte [batYNstr + 3]
    call ucChar         ;UC using DOS
    cmp al, "Y"
    je .killBat
    cmp al, "N"
    jne .ynLp
.endBat:
;Tries to abort the Command interpreter, fails and 
; our internal state will proceed with batch processing. Doing this 
; ensures the command being interrupted is cancelled.
    call printCRLF
    jmp .exitBat    ;Clears errRetHdls and in ctrlc flag and exits!
.killBat:   ;Now we need to terminate the batch file too.
    call batCleanup
    jmp short .endBat   ;Now CRLF and exit!
.notBat:
    push rax        ;Save rax, ah contains call we are in that ^C'ed
    push rbx
    mov eax, 5100h  ;Get current PSP in rbx
    int 21h
    pop rax
    cmp rbx, qword [pspPtr] ;Was the task us?
    pop rbx
    jne .exit       ;If not, then immediately abort it!
    test byte [permaSwitch], -1 ;If not permanent, skip parent PSP swap
    jnz .noJuggle   ;Avoid setting "real parent"
    ;Else, we juggle parent PSP's. This is done when /P is not specified
    push rax
    push rbx
    mov rax, qword [realParent]
    mov rbx, qword [pspPtr]
    mov qword [rbx + psp.parentPtr], rax    ;Store the parent there
    pop rbx
    pop rax
.noJuggle:
    cmp word [errHdls], -1  ;If these are not -1, return to normal!
    je .exit
.exitBat:
    call errRetHdls 
.exit:
    and byte [statFlg1], ~inCtrlC
.exitIn:
    stc     ;Set CF to kill the task
    ret 8   ;Return and adjust rsp stack to indicate we wanna kill task
;All we need is that the stack pointer is not at the address we had it
; at when we entered DOS to trigger a CF check! So anything from ret 8 to
; ret 4*8 to remove CS, RFLAGS, SS, RSP is acceptable and safe!