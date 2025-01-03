;COMMAND.COM main file
[map all ./lst/command.map]
[DEFAULT REL]
BITS 64
;Defs, strucs and macros
;Used to ensure that the init on disk and bss virtually are aligned equally
cmdAlign    equ 10h 

%include "./inc/dosMacro.mac"
%include "./inc/dosStruc.inc"
%include "./inc/fcbStruc.inc"
%include "./inc/dosError.inc"
%include "./inc/cmdEqu.inc"

Segment cmd align=1 valign=1
%define currSegVBase 0
%include "./dat/cmdData.asm"
%include "./dat/cmdTable.asm"
%include "./dat/cmdStr.asm"
%include "./src/cmdMain.asm"
%include "./src/cmdBat.asm"
%include "./src/cmdFunc.asm"
%include "./src/cmdUtils.asm"
%include "./src/int23h.asm"
%include "./src/int24h.asm"

Segment bss nobits valign=cmdAlign vfollows=cmd
%include "./dat/cmdBss.asm"
bssLen equ ($ - $$)

Segment stack1 nobits valign=16 vfollows=bss
    dq 200 dup (?)  ;4Kb stack, para aligned
stackTop:   ;Top of the stack
    dq ?    ;Extra paragraph
    dq ?
endOfAlloc: ;Symbol to free from once init is over!
stack1Len equ ($ - $$)

Segment init align=cmdAlign valign=cmdAlign follows=cmd vfollows=stack1
%define currSegVBase section.init.vstart
%include "./src/cmdLdr.asm"
endOfInitAlloc: ;Symbol to free during init
initLen equ ($ - $$)
initOffset  equ bssLen + stack1Len  ;Amount of space to make for bss/stack
