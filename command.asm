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
%include "./src/cmdErr.asm"
%include "./src/cmdFunc1.asm"
%include "./src/cmdFunc2.asm"
%include "./src/cmdUtils.asm"
%include "./src/int23h.asm"
%include "./src/int24h.asm"

Segment bss nobits align=cmdAlign follows=cmd
%include "./dat/cmdBss.asm"
    alignb 10h
bssLen equ ($ - $$)

Segment stack nobits align=16 follows=bss
    dq 200 dup (?)  ;1.6K stack, para aligned
stackTop:   ;Top of the stack
    dq ?    ;Extra paragraph
    dq ?
endOfAlloc: ;Symbol to free from once init is over!
stackLen equ ($ - $$)

Segment init align=cmdAlign valign=cmdAlign follows=cmd vfollows=stack
%define currSegVBase section.init.vstart
%include "./src/cmdLdr.asm"
endOfInitAlloc: ;Symbol to free during init
initLen equ ($ - $$)

;Amount of space to make for bss/stack
initOffset  equ bssLen + stackLen  
