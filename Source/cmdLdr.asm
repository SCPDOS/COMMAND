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
    or byte [statFlg1], inLdr  ;Ok now we start our special work
    mov qword [pspPtr], r8  ;Store PSP ptr in internal var 
    mov rax, qword [r8 + psp.parentPtr] ;Get PSP parent
    mov qword [r8 + psp.parentPtr], r8  ;Store self as parent
    mov qword [realParent], rax ;Preserve the real parent address
;Setup Int 22h, Int 23h and Int 24h
    mov rax, qword [r8 + psp.oldInt22h] ;Preserve the original addresses
    mov qword [parentInt22], rax
    call resetIDTentries
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

;Now parse the command tail. 
;COMMAND [drive:][path][device][/E:n][/P][/C string]
;[drive:] gives the default drive for COMSPEC. Default is default drive.
;[path] gives the search path to search for when searching for COMSPEC.
;   Default is root directory. Hence default COMSPEC is _:\COMMAND.COM
;[device] gives the default device to set STDIO to and reset to if we error
;           MAX 8 chars long name ([device] can be colon terminated).
;           Default is CON
;[/C string] means execute the string as a command and terminate immediately
;   This is single command mode, sets inSingle bit. Similar to int 2Eh.
;[/P] means make the instance of COMMAND.COM permanent.
;[/E:n] where n is a base 10 value between 160-32768, 
; giving the size of the environment to allocate in bytes. Only works if
; COMMAND.COM is to be permanent. Needs a colon after E. First digit 
; immediately after the colon, no space.

;If permanent not set, determine if this is the initial/master copy of 
; COMMAND.COM by check if Int 2Eh has the same address as Int 2Dh. 
;If so, we are master.
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
    mov byte [autoSpec], al 
;Set Int 2Eh up
    lea rdx, int2Eh
    mov eax, 252Eh ;Set this as Int 2Eh
    int 21h
;Now allocate the master environment!
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
;Now, open AUTOEXEC.BAT. 
    lea rdx, autoSpec
    mov eax, 3D00h  ;Open file
    int 21h
    jc .noAutoexec
    ;Ok, we know the file exists, close it and finish init through it, ignoring 
    ; the normal hello there! string
    mov ebx, eax
    mov eax, 3E00h  ;Close file
    int 21h
;Now we do the same as at the end, prepping for jettisoning
    call computeStackPtr    ;Returns the stack ptr in rbx and var
    mov rsp, rbx        ;Move the stack pointer to this address
    and byte [statFlg1], ~inLdr    ;Special work complete :-)
    xor edx, edx
    dec edx             ;Setup that we want to process Autoexec
    jmp commandStart
.noAutoexec:
;If no AUTOEXEC.BAT, request time and date from user
    lea rdx, crlf
    mov ah, 09h
    int 21h
    or byte [statFlg1], inLdrDT
    call time
    call date
    and byte [statFlg1], ~inLdrDT
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
    movzx eax, byte [rdi]   ;rdi points to the char after the switch
    call ucChar
    cmp al, "P" ;Is it permanent switch?
    jne .printInit
    mov byte [permaSwitch], -1  ;Set the permanently resident switch on
.printInit:
;Finish by printing INIT string.
    lea rdx, initString
    mov ah, 09h
    int 21h ;Print init string
    call version.printVersionNumber
    lea rdx, initString2
    mov ah, 09h
    int 21h ;Print init string
    call computeStackPtr    ;Returns the stack ptr in rbx and var
    mov rsp, rbx        ;Move the stack pointer to this address
    and byte [statFlg1], ~inLdr    ;Special work complete :-)
    xor edx, edx        ;Indicate we DONT want to do Autoexec processing
    jmp commandStart    ;We jump with rbx = base address to jettison
;Loader Data here
initString: 
    db CR,LF,"Scientific Computer Research(R) SCP/DOS(R) Version $"
initString2:
    db CR,LF, "          (C)Copyright Scientific Computer Research 2024.",CR,LF,"$"
badVerStr: db "Incorrect DOS version",CR,LF,"$"

computeStackPtr:
    ;Now we add the stack to the alloc and paragraph align
    lea rbx, endOfAlloc
    add rbx, stackSize
    add rbx, 11h    ;Go one para up
    shr rbx, 4      ;Round to this new para boundary
    shl rbx, 4
    mov qword [stackTop], rbx   ;Save this value of the stack ptr in var
    return