;COMMAND.COM main file
[map all ./Listings/command.map]
[DEFAULT REL]
BITS 64
;Defs, strucs and macros
%include "./Include/dosMacro.mac"
%include "./Include/dosStruc.inc"
%include "./Include/fcbStruc.inc"
%include "./Include/dosError.inc"
%include "./Include/cmdEqu.inc"
Segment .data align=1 
%include "./Data/cmdData.asm"
%include "./Data/cmdTable.asm"
%include "./Data/cmdMsg.asm"
%include "./Data/cmdEnv.asm"
%include "./Source/cmdMain.asm"
%include "./Source/cmdFunc.asm"
%include "./Source/cmdUtils.asm"
%include "./Source/int24h.asm"
endOfAlloc: ;End of alloc
;The stack is always setup one stackSize away from the endOfAlloc, aligned to
Segment transient align=1 follows=.data
;This segment always gets ejected post load
%include "./Source/cmdLdr.asm"

stackSize equ 200*8    ;200 QWORD stack