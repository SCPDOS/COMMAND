cmdLdr:
;First check if the version is ok. If not, return.
    mov ah, 30h
    int 21h
    cmp al, 01h ;Version 1
    jbe .okVersion
    lea rdx, badVerStr
    mov ah, 09h
    int 21h
.exitBad:
    int 20h ;Exit to caller or DOS to print bad command interpreter line
.okVersion:
;If ok then store self as parent in the PSP, to prevent accidental closure
    mov qword [pspPtr], r8  ;Store PSP ptr in internal var 
    mov rax, qword [r8 + psp.parentPtr] ;Get PSP parent
    mov qword [r8 + psp.parentPtr], r8  ;Store self as parent
    mov qword [realParent], rax ;Preserve the real parent address
;Setup Int 22h, Int 23h and Int 24h
    mov rax, qword [r8 + psp.oldInt22h] ;Preserve the original addresses
    mov qword [parentInt22], rax

    lea rdx, critErrorHandler
    mov qword [r8 + psp.oldInt24h], rdx
    mov eax, 2524h
    int 21h
    lea rdx, int23h
    mov qword [r8 + psp.oldInt23h], rdx
    mov eax, 2523h
    int 21h
    lea rdx, launchChild.appRet
    mov qword [r8 + psp.oldInt22h], rdx
    mov eax, 2522h
    int 21h
;Get a pointer to DOS Sysvars
    mov ah, 52h ;Get sysvars
    int 21h
    mov qword [sysVars], rbx    ;Save ptr to sysVars
;Call for simple internationalisation data
    mov eax, 3700h  ;Get switchchar in dl
    int 21h
    cmp al, -1
    je .skipSwitch
    mov byte [switchChar], dl   ;Store the switchChar in var
    cmp dl, "-" ;Is the switchChar Unix?
    jne .skipSwitch
    mov byte [pathSep], "/" ;Swap default path separator to UNIX style
.skipSwitch:
    mov eax, 3800h  ;Get current country data
    lea rdx, ctryData
    int 21h ;Write the data to the internal country table
;Now determine if this is the master copy of COMMAND.COM
;Check if Int 2Eh has the same address as Int 2Dh. If so, we are master.
    mov eax, 352Eh  ;Get int 2Eh address
    int 21h
    mov rdx, rbx    ;Save the pointer in rdx
    mov eax, 352Dh  ;Get int 2Dh address
    int 21h
    cmp rdx, rbx    ;If these are equal then this is first boot!
    jne .skipMaster
;Ok so we are master command.com
;Now make myself the real parent
    mov byte [permaSwitch], -1  ;Set the permanently resident switch on
    mov qword [realParent], r8
;Set current Drive in COMSPEC
    mov eax, 1900h ;Get current Drive
    int 21h
    add al, "A"
    mov byte [comspecEvar.cspec], al
;Set Int 2Eh up
    lea rdx, int2Eh
    mov eax, 252Eh ;Set this as Int 2Eh
    int 21h
;Now, open and parse AUTOEXEC.BAT. Build new Master Environment here.
;If no AUTOEXEC.BAT, request time and date from user
    mov ebx, 10 ;Allocate 160 bytes
    mov eax, 4800h
    int 21h
    jc .exitBad
;Set master environment as mine
    mov qword [r8 + psp.envPtr], rax
    lea rsi, masterEnv
    mov rdi, rax
    mov ecx, menv_len
    rep movsb   ;Copy the chars over!

    lea rdx, crlf
    mov ah, 09h
    int 21h
    call time
    call date
    lea rdx, crlf
    mov ah, 09h
    int 21h
    jmp short .printInit
.skipMaster:    
;We now search for the master environment!!
    ;Walk the parentPSP chain until we find a PSP which is its own
    ; parent. This is the previous master command interpreter! This means
    ; that non-master COMMAND.COM instances are not their own parents! We 
    ; then set our environment ptr in the PSP to the master environment.
    lea rdi, qword [r8 + psp.progTail]
    movzx ecx, byte [r8 + psp.parmList]
    movzx eax, byte [switchChar]
    repne scasb
    jecxz .printInit
    movzx eax, byte [rdi]   ;RDI points to the char after the switch
    call ucChar
    cmp al, "P" ;Is it permanent switch?
    jne .printInit
    mov byte [permaSwitch], -1  ;Set the permanently resident switch on
.printInit:
    lea rbx, endOfAlloc ;Save the Master Environment
;Finish by printing INIT string.
    push rbx
    lea rdx, initString
    mov ah, 09h
    int 21h ;Print init string
    call version.printVersionNumber
    lea rdx, initString2
    mov ah, 09h
    int 21h ;Print init string
    pop rbx
    ;Now we add the stack to the alloc and paragraph align
    add rbx, stackSize
    add rbx, 11h    ;Go one para up
    shr rbx, 4      ;Round to this new para boundary
    shl rbx, 4
    mov rsp, rbx    ;Move the stack pointer to this address
    mov qword [stackTop], rbx   ;Save this value of the stack ptr in var
    jmp commandStart    ;We jump with rbx = base address to jettison
;Loader Data here
initString: 
    db CR,LF,"Scientific Computer Research(R) SCP/DOS(R) Version $"
initString2:
    db CR,LF, "          (C)Copyright Scientific Computer Research 2024.",CR,LF,"$"
badVerStr: db "Incorrect DOS version",CR,LF,"$"