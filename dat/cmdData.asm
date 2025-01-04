;Static Data Area for COMMAND.COM    
startLbl:   ;Start symbol, this is the entry point
pPSP:       ;Internal pointer to the task PSP
    jmp cmdLdrE
    db 3 dup (0CCh) ;So add three bytes to turn it into a variable after use
numHdls     dw 20   ;Get number of handles permitted
pathSep     db "\"  ;Default path sep
switchChar  db "/"  ;Default switch char
failDrv     db -1   ;0 based drive number. Used to identify if drv bad
echoFlg     db 1    ;Global Echo flag, starts up! 1 means on
errHdls     dw -1   ;Set to the STDIO handles. Non -1 => Handles swapped
;Volume static FCB for filesearches
volFcb:
    istruc exFcb
    at exFcb.extSig,    db -1   ;Indicate extended FCB
    at exFcb.attribute, db dirVolumeID
    at exFcb.driveNum,  db 0    ;Current drive
    at exFcb.filename,  db "????????"
    at exFcb.fileext,   db "???"
    at exFcb.curBlock,  dd 0
    iend 

;The string for Y/N prompt for batch ^C handler
batYNstr    db 2,1," ",CR           ;String for buffered Y/N input
;==============================================================================
; Do not split the blocks below!
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cmdLineStatePtr:
pipeFlag    db 0    ;If set, we fired up a pipe for this command line
pipeSTDIN   dw -1   ;The handle to replace STDIN with once all piping complete
pipeSTDOUT  dw -1   ;The handle to replace STDOUT with once all piping complete
;These variables are valid for a SINGLE command in a command line
;Next two bytes, if set to -1, flags error
redirIn     db 0    ;If set, we are redirecting input from a file
redirOut    db 0    ;If 1, we are redirecting output to a file, destructively
;                    If 2, we are redirecting output to a file, by appending
redirSTDIN  dw -1   ;The handle to replace STDIN with once redir complete
redirSTDOUT dw -1   ;The handle to replace STDOUT with once all redir complete
;------------------------------------------------------------------------------
cmdStatePtr:   ;Symbol to use for clearing command state variables
arg1Flg     db 0    ;Set if there was a first argument
arg1Off     db 0    ;Offset into cmdBuffer to the argument
arg1FCBret  db 0    ;AL on return from parse filename for argument 1

arg2Flg     db 0    ;Set if there was a second argument
arg2Off     db 0    ;Offset into cmdBuffer to the argument
arg2FCBret  db 0    ;AL on return from parse filename for argument 2

switchFnd   db 0    ;Set if a switch char is found
cmdStateL equ $ - cmdStatePtr
cmdLineStateL equ $ - cmdLineStatePtr
;------------------------------------------------------------------------------
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++