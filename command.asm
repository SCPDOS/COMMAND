;COMMAND.COM main file
[map all ./Listings/command.map]
[DEFAULT REL]
BITS 64
;Defs, strucs and macros
stackSize equ 200*8    ;200 QWORD stacks always
%include "./Include/dosMacro.mac"
%include "./Include/dosStruc.inc"
%include "./Include/fcbStruc.inc"
%include "./Include/dosError.inc"
%include "./Include/cmdEqu.inc"
Segment cmd align=1 valign=1
%include "./Data/cmdData.asm"
%include "./Data/cmdTable.asm"
%include "./Data/cmdStr.asm"
%include "./Source/cmdMain.asm"
%include "./Source/cmdBat.asm"
%include "./Source/cmdFunc.asm"
%include "./Source/cmdUtils.asm"
%include "./Source/int23h.asm"
%include "./Source/int24h.asm"
endOfAlloc: ;End of alloc
%include "./Source/cmdLdr.asm"
Segment stack1 nobits valign=16 vfollows=cmd
;This stack is only used during init of COMMAND.COM
    dq stackSize dup (?)
initEoA:    ;Initial end of allocation
