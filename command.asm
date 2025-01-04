;COMMAND.COM main file
[map all ./lst/command.map]
[DEFAULT REL]
BITS 64
;Defs, strucs and macros

%include "./inc/dosMacro.mac"
%include "./inc/dosStruc.inc"
%include "./inc/fcbStruc.inc"
%include "./inc/dosError.inc"
%include "./inc/cmdEqu.inc"

    ORG 100h    ;Allows for r8 to be used as a base pointer for section reloc
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
cmdLdrE:

Segment bss nobits align=1 follows=cmd
%include "./dat/cmdBss.asm"
bssLen equ ($ - $$)

Segment stack nobits align=1 follows=bss
    alignb 10h
    dq 200 dup (?)  ;1.6K stack, para aligned
stackTop:   ;Top of the stack
    dq ?    ;Extra paragraph
    dq ?
endOfAlloc: ;Symbol to free from once init is over!
stackLen equ ($ - $$)

Segment init align=1 valign=1 follows=cmd vfollows=stack
%define currSegVBase section.init.vstart
%include "./src/cmdLdr.asm"
endOfInitAlloc: ;Symbol to free during init
initLen equ ($ - $$)

;Amount of space to make for bss/stack
initOffset  equ bssLen + stackLen  
