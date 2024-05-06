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
    or byte [statFlg1], inLdr   ;Ok now we start our special work
    mov qword [pspPtr], r8      ;Store PSP ptr in internal var 
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
;Now eject all the unneeded space, to make space for allocating.
;Thus the allocations in the parsing should never fail (but still may)
    lea rsp, initEoA
    mov rbx, rsp
    neg r8  ;Convert r8 to -r8
    lea rbx, qword [rbx + r8]    ;Get # of bytes for COMMAND.COM and stack
    shr ebx, 4  ;Convert to paragraphs
    mov eax, 4A00h ;Realloc
    neg r8  ;Convert -r8 to r8
    int 21h
    jc .exitBad 
;Now space has been made, time to parse the command tail
    call parseCmdLine   ;Now parse the command tail.
    call doEnv          ;Now enact the command tail actions
    call resetIDTentries    ;Setup IDT entries now. 
    test byte [statFlg1], permaShell ;Are we perma?
    jz .notMaster   ;Jump if not!
;Ok so we are master COMMAND.COM
;Now make myself my own parent task. We already are the current psp
    mov qword [r8 + psp.parentPtr], r8
;Setup Int 2Eh and the addresses in my psp to terminate back to
    lea rdx, critErrorHandler
    mov qword [r8 + psp.oldInt24h], rdx
    lea rdx, ctrlCHandler
    mov qword [r8 + psp.oldInt23h], rdx
    lea rdx, appRet
    mov qword [r8 + psp.oldInt22h], rdx
    lea rdx, int2Eh
    mov eax, 252Eh ;Set this as Int 2Eh
    int 21h
;Always get the Autoexec from the root dir we are booting into
    call getCurrentDrive    ;Get 0 based drive number in al
    add al, "A"
    mov byte [autoSpec], al ;
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
    xor edx, edx
    dec edx             ;Setup that we want to process Autoexec
    jmp .prepStart
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
    ;Now fall thru.
.notMaster:    
;Finish by printing INIT string.
    test byte [statFlg1], inSingle  ;Are we in single mode?
    jnz .singleCom
    lea rdx, initString
    mov ah, 09h
    int 21h ;Print init string
    call version.printVersionNumber
    lea rdx, initString2
    mov ah, 09h
    int 21h ;Print init string
    xor edx, edx        ;Indicate we DONT want to do Autoexec processing
.prepStart:
    call computeStackPtr    ;Returns the stack ptr in rbx and var
    mov rsp, rbx        ;Move the stack pointer to this address
    and byte [statFlg1], ~inLdr    ;Special work complete :-)
    jmp commandStart    ;We jump with rbx = base address to jettison
.singleCom:
;In single command mode, check the length of the input string was not 0.
; If it was, exit, else proceed
    cmp byte [inBuffer + 1], 0
    je exit
    xor edx, edx
    dec edx             ;Pretend that we want to process Autoexec
    jmp short .prepStart

computeStackPtr:
    ;Now we add the stack to the alloc and paragraph align
    lea rbx, endOfAlloc
    add rbx, stackSize
    add rbx, 11h    ;Go one para up
    shr rbx, 4      ;Round to this new para boundary
    shl rbx, 4
    mov qword [stackTop], rbx   ;Save this value of the stack ptr in var
    return

parseCmdLine:
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
;[/E:n] where n is a base 10 value between 160-32768, giving the size of the 
; environment to allocate in bytes. Only works if COMMAND.COM is to be 
; permanent. Needs a colon after E. 
;
;Start by setting up the default comspec dir, null terminated
    call getCurrentDrive    ;Get 0 based drive number in al
    add al, "A"
    mov ah, ":"
    lea rdi, comspecDir
    stosw   ;Store drive specifier
    movzx eax, byte [pathSep]
    stosw   ;Store the pathsep and a terminating <NUL>

    call setDTA                     ;Ensure any searches dont trample on the tail!
    lea rsi, qword [r8 + cmdLine]   ;Get the start of tail chars
.parseLp:
    call skipDelimiters             ;Strip delims
    lodsb
    dec rsi ;Point to first non-delimiter char
    cmp al, CR
    je .endScan
    cmp al, byte [switchChar]       ;Is this a switchchar?
    jne .file
.switch:
    inc rsi ;Go past the switch char
    lodsb   ;Get first char past switch, advance rsi
    call ucChar
    cmp al, "C"
    je .switchStr
    cmp al, "P"
    je .switchPerma
    cmp al, "E"
    je .switchEnv
    dec rsi ;Now point rsi back to this char
.badparm:
    lea rdx, badParm
    jmp short .parseLp
;-----------------------------------------------------------------
;Search for the filespec
.file:
    call copyArgumentToSearchSpec   ;Moves rsi to char past the delimiter
    dec rsi ;Point to the delimiter itself
    lea rdx, searchSpec
    mov eax, 4E00h  ;Find First
    mov ecx, dirDirectory   ;Return Normal, RO, Dir or Char
    int 21h
    jnc .charOrDir
.badSpec:
;rsi has been moved past the argument so we can keep proceeding with processing
    lea rdx, badCmdDir
.bs0:
    call printString
.plhop:
    jmp short .parseLp
;-----------------------------------------------------------------
;Check if a char or directory here
.charOrDir:
    mov al, byte [cmdFFBlock + ffBlock.attribFnd]
    and al, dirCharDev | dirDirectory
    jz .badSpec
    test al, dirCharDev
    jnz .charDev
;Here if a directory. Save it null terminated in comspecDir.
    push rsi
    lea rsi, searchSpec 
    lea rdi, comspecDir
    call strcpy2
    mov eax, 1211h  ;Uppercase the pathspec
    int 2Fh         ;DOS 3.3 does't do this hehe 0:-)
    pop rsi
    mov byte [initNewSpec], -1  ;Set, we have a new comspec
    jmp short .plhop
;-----------------------------------------------------------------
;Handle char dev here
.charDev:
;Check the name is max 8 chars in length
    lea rdi, searchSpec
    call strlen ;Get len plus <NUL> in ecx. Cant be more than 8+1 for <NUL>
    cmp ecx, 9  ;This can literally never happen but never hurts to be safe :)
    ja .badChar
    push rsi    ;Save offset into cmdTail on stack
    lea rsi, searchSpec
    lea rdi, devName
    call strcpy ;Copy this string over
    pop rsi
    mov byte [initNewDev], -1   ;Set this byte now we have a new dev!
.plhop1:
    jmp short .plhop    ;parseLp
.badChar:
    lea rdx, badDev
.bs1:
    jmp short .bs0
;-----------------------------------------------------------------
.switchEnv:
    lodsb
    dec rsi
    cmp al, ":"
    jne .badparm
    inc rsi ;Now point past teh colon, skip any delimiters
    call skipDelimiters
    lodsb       ;Get the first non-delimiter
    dec rsi     ;Point back at it
    cmp al, CR
    je .badparm     ;Print bad arg, rsi positioned to terminate parsing now
    ;Else, rsi now pointing at the first number. Do maths :)
    call getNum ;Move rsi to first non digit/9th digit if too many digits
    jc .badparm    ;Bad parameter error!
    cmp eax, 8000h
    ja .badEnv
    cmp eax, 0A0h
    jb .badEnv
    mov word [initEnvSz], ax    ;Store the allocated size here
.plhop2:
    jmp short .plhop1  ;parseLp
.badEnv:
    lea rdx, badEnvSz
    jmp short .bs1
;-----------------------------------------------------------------
.switchPerma:
    lodsb   ;Get the second char after the P
    dec rsi ;Point back at it
    cmp al, CR
    je .spEmbCr ;Allow CR after the P
    call isALdelimiter
    jne .badparm
.spEmbCr:
    or byte [statFlg1], permaShell  ;Set the bit
    jmp short .plhop2  ;parseLp
;-----------------------------------------------------------------
.switchStr:
    lodsb       ;Get the char after the C
    dec rsi     ;Point back at it
    cmp al, CR
    je .ssembCr         ;Embedded CR ok
    call isALdelimiter  ;Char immediately after the C must be a delimiter
    jne .badparm
.ssembCr:
    call skipDelimiters     ;Move rsi past the delimiters
    lea rdi, inBuffer + 2   ;Store the command in the inBuffer as if typed in
.ssLp:
    lodsb
    stosb
    cmp al, CR
    je .ssOk
    inc byte [inBuffer + 1] ;Increment the char count
    jmp short .ssLp
.ssOk:
    or byte [statFlg1], inSingle    ;Set the single flag
;Now we fall through as we have processed the cmd line and set the flag
;-----------------------------------------------------------------
.endScan:
;Now copy our name to the end of the dir specified
    lea rdi, comspecDir
    call strlen ;Get the length in ecx, preserve rdi
    dec ecx ;Point to null terminator
    mov al, byte [pathSep]
    add rdi, rcx    ;Now point to the null
    cmp byte [rdi - 1], al
    je .esSkipPathsep
    stosb
.esSkipPathsep:
    lea rsi, cspec
    call strcpy ;Now copy over the name of our beloved shell
;Now ensure the user hasnt specified /C on the master shell!
    call .masterCheck
    call .replacestdio
;Ensure that on exit, if both inSingle bit and permaShell are set, inSingle 
; wins. Unless this is the first boot, in which case, we ignore it
    test byte [statFlg1], inSingle
    retz    ;Return if not set
    and byte [statFlg1], ~permaShell ;Else, ensure perma is deactivated
    return
.masterCheck:
;Before exiting, determine if this is the initial/master copy of
; COMMAND.COM by checking if Int 2Eh has the same address as Int 2Dh. 
;If so, we are master, set the permanent bit.
    mov eax, 352Eh  ;Get int 2Eh address
    int 21h
    mov rdx, rbx    ;Save the pointer in rdx
    mov eax, 352Dh  ;Get int 2Dh address
    int 21h
    cmp rdx, rbx    ;If these are equal then this is first boot!
    retne   ;Else, we are done. Return
    or byte [statFlg1], permaShell
    mov al, byte [statFlg1]
    and byte [statFlg1], ~inSingle  ;Ensure this bit is off
    test al, inSingle   ;Was the bit initially set?
    retz                ;Return if not
    lea rdx, badCombo   ;Else, sorry buddy, please type in your command manually!
    call printString
    return
.replacestdio:
    test byte [initNewDev], -1
    retz    ;Return if not set
    lea rdx, devName
    jmp ctty.loadSwap  ;Return through the return instruction in ctty

doEnv:
;Will do the environment adjustments as needed
    mov rsi, qword [r8 + psp.envPtr]
    test rsi, rsi
    jz .noInitEnv
    cmp word [initEnvSz], 0 ;If an env exists and /E not specified, return!
    retz
;Now if no new comspec has been given, we allocate a new block, 
;copy the environment from the master and free the original block.
;If the block we allocate is smaller than the original block, 
; print out of env space and stop there
;Else, we simply allocate and create a fresh new environment with the new 
; comspec. 
    test byte [initNewSpec], -1  ;If we have a new comspec, create afresh
    jnz .prepNewEnv
    ;Else, we create new alloc and copy master into it
    call .allocEnv
    mov rdi, rax        ;Move pointer into rdi
    call getMasterEnv   ;Get the current active master environment in rsi
    push rdi            ;Save the start of the environment block
    call .cpEnv         ;Copy the environment
    pop rdi
    call .freeCurrentEnv
    mov qword [r8 + psp.envPtr], rdi    ;Now set us as the env ptr
    return  
.freeCurrentEnv:
    push r8
    mov r8, qword [r8 + psp.envPtr] ;Get the original envPtr to free
    mov eax, 4900h
    int 21h
    pop r8
    return
.prepNewEnv:
    call .freeCurrentEnv
.noInitEnv:
;Build the initial block.
    call .allocEnv
    mov rdi, rax        ;Move rdi to the environment
    mov qword [r8 + psp.envPtr], rdi    ;Now store this as the env pointer
    push rdi            ;Save the environment ptr!
    lea rsi, pathEVar
    call strcpy
    lea rsi, comspecEVar
    call strcpy
    dec rdi ;Write over the terminating null
    lea rsi, comspecDir    ;Now copy the actual comspec! 
    call strcpy 
    xor al, al
    stosb   ;Store the second terminating null
    pop rdi ;Get back the pointer to the start of the env
    return

.allocEnv:
;Output: rax -> Ptr to the environment block. 
;   If anything goes wrong, error message and exit the shell!
    movzx ebx, word [initEnvSz]
    test ebx, ebx   ;If this is 0, allocate 160 bytes
    jnz .goAlloc
    mov ebx, 0A0h    ;160 bytes
    mov word [initEnvSz], bx    ;Overwrite
.goAlloc:
    add ebx, 0Fh
    shr ebx, 4  ;Round up and turn into paragraphs
    mov eax, 4800h
    int 21h
    retnc
    lea rdx, memBad1
    call printString
    jmp exit.ouch 

.cpEnv:
;Copies the environment
;Input: rsi -> Source to copy from
;       rdi -> Destination to copy to
    movzx ecx, word [initEnvSz]
.lp:
    lodsb
    stosb
    test al, al
    jnz .notNul
    cmp byte [rsi], al
    jnz .notNul
    ;Here, we have encountered our double null!
    dec ecx ;Now we check if we have space to store the second char
    jz .err ;If we dont have space for the last null kill last non-null char
    stosb
    return
.notNul:
    dec ecx ;One less char to copy over
    jnz .lp
.err:
    sub rdi, 2
    lea rdx, noEnvSpace
    call printString
    xor eax, eax
    stosw   ;Store 0 word
    return

getMasterEnv:
;Walks the psp chain to the first self parent.
;Output: rsi -> Environment of the first self-parent program
    push r8
.lp:
    mov rsi, qword [r8 + psp.parentPtr] ;Get the parent pointer
    cmp rsi, r8 ;Are we a self parent?
    je .exit
    mov r8, rsi ;Make the current psp the parent
    jmp short .lp
.exit:
    pop r8
    mov rsi, qword [rsi + psp.envPtr]   ;Get the environment pointer
    return


;Loader Data here
initString: 
    db CR,LF,"Scientific Computer Research(R) SCP/DOS(R) Version $"
initString2:
    db CR,LF, "          (C)Copyright Scientific Computer Research 2024.",
    db CR,LF,"$"
badVerStr:  db "Incorrect DOS version",CR,LF,"$"
badCmdDir:  db "Specified COMMAND search directory bad",CR,LF,"$"
badCombo:   db "Cannot specify /C on top level process. Ignoring...",CR,LF,"$"
badEnvSz:   db "Invalid Environment Size",CR,LF,"$"
initNewSpec db 0    ;Set if a new comspec found and copied
initNewDev  db 0    ;Set if a new device found and copied
initEnvSz       dw 0  ;Max 7FFFh (32768) bytes. Default to 160. 0 means no /E: