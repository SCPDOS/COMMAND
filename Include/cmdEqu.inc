;Data equates and struct declarations

cmdBufferL  equ 130 ;Length of a command line buffer (add two bytes for padding)
driveSpecL  equ 2   ;Space for X:
dirPathL    equ 64  ;Length of a directory path
fcbNameL    equ 11  ;8.3 => 11 chars in name
filenameL   equ fcbNameL + 1 ;Add a dot for the name.ext separator
fileNameZL  equ filenameL + 1   ;Add a space for a terminating null
fullDirPathL equ driveSpecL + dirPathL
fullDirPathZL equ fullDirPathL + 1   ;Add one for terminating null
fileSpecZL  equ fullDirPathL + fileNameZL ;One full asciiz pathspec
cmdNameL    equ 1 + fileNameZL  ;1 for the count prefix

;Use PSP FCB's for switch buffers
fcb1        equ psp.fcb1
fcb2        equ psp.fcb2
;Use the dta as the built command line buffer.
cmdTail     equ psp.parmList
cmdLineCnt  equ psp.parmList
cmdLine     equ psp.progTail


;Struct Declarations
;In batch, for pointers, 0 means NULL.
;FOR/CALL not priorities for implementation right now.
struc batBlockHdr
    .zero       db ?    ;Always null
    .bEchoFlg   db ?    ;Echo flag on entry to batch. 1 if echo on!
    .pCallBlk   dq ?    ;Pointer to the previous batch block when CALLed
    .pForBlk    dq ?    ;Pointer to the active FOR block
    .bBatFlg    db ?    ;Indicates type of command currently active
    .dBatOffLo  dd ?    ;LSEEK Low dword of the batch offset
    .dBatOffHi  dd ?    ;LSEEK High dword of the batch offset (0 unless net FS)
    .wArgs      dw 10 dup (?) ;Offsets of parameters %0-%9. -1 means no param
    .cmdLine:   ;0 terminated path (%0) followed by cmd line params.
    ;We include terminating CR. Add null at the end to mean end of params.
endstruc
bbNoParam   equ -1
bbNull      equ 0
bbEchoOn    equ 1
bbEchoOff   equ 0
bbMaxAlloc  equ batBlockHdr_size + cmdBufferL + 1 ;Add 1 for good measure :)
batNoEchoChar   equ "@" ;If a line starts with this, no echo!



struc drvHdr  ;Device Driver Header for character and block devices
    .nxtPtr dq ?  ;Pointer to the next driver header, -1 if at the end
    .attrib dw ?  ;Attribute Word
    .strPtr dq ?  ;Strategy Entry Pointer
    .intPtr dq ?  ;Interrupt Entry Pointer
    .drvNam db 8 dup (?)  ;Driver name (Char) or Number of units byte (Block)
endstruc

struc execProg  ;For use with EXEC-ing a child task
    .pEnv       dq ?  ;Ptr to environment block (or 0 => copy parent env)
    .pCmdLine   dq ?  ;Ptr to the command line to be placed at PSP + 80h
    .pfcb1      dq ?  ;Ptr to the first FCB (parsed argument 1)
    .pfcb2      dq ?  ;Ptr to the second FCB  (parsed argument 2)
endstruc

;Directory attribute equates
    dirReadOnly     equ 01h
    dirHidden       equ 02h
    dirSystem       equ 04h
    dirVolumeID     equ 08h
    dirDirectory    equ 10h
    dirArchive      equ 20h
    dirCharDev      equ 40h ;Never written to disk, used to represent a Char Dev
    dirLongName     equ dirReadOnly | dirHidden | dirSystem | dirVolumeID
    ;If any of the three bits are set, then ALL three bits are set
    ; in addition to whatever the user passed to search for.
    dirInclusive    equ dirHidden | dirSystem | dirDirectory
    dirIncFiles     equ dirHidden | dirSystem