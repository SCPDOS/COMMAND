;Static Data Area for COMMAND.COM    
startLbl:   ;Start symbol, this is the entry point
    jmp cmdLdr
stackTop    dq 0    ;Pointer to the top of the stack as allocated by DOS
returnCode  dw 0    ;Return Code from a child process
pspPtr      dq 0    ;Internal pointer to the task PSP
int2Epsp    dq 0    ;
int2Ersp    dq 0    ;Save the far Int 2E entry stack pointer!
numHdls     dw 20   ;Get number of handles permitted
pathSep     db "\"  ;Default path sep
switchChar  db "/"  ;Default switch char
;Static strings, not used in command line parsing
ctryData    db countryStruc_size dup (0)  ;Length of the country table
currDirStr  db fullDirPathZL dup (0) ;Current Directory String
;=============================================================
statFlg1    db 0    ;Flags 1 for the command interpreter
;=============================================================
permaShell  equ 1   ;Up if we are a permanent command interpreter
inCtrlC     equ 2   ;Up if we are processing int 23h
inSingle    equ 4   ;Up if processing a single command (/C mode)
inBatch     equ 8   ;Up if processing a batch file
batchEOF    equ 10h ;Set to indicate we have reached the end of the batch file
inCritical  equ 20h ;Up if processing a command.
inLdrDT     equ 40h ;Up if in the Date / Time part of Loader
inLdr       equ 80h ;Up if in loader
failDrv     db -1   ;0 based drive number. Used to identify if drv bad
echoFlg     db 1    ;Global Echo flag, starts up! 1 means on
errHdls     dw -1   ;Set to the STDIO handles. Non -1 => Handles swapped
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
;Batch state variables.
bbPtr       dq 0    ;Ptr to the batch block
batFile     db fileSpecZL dup (0)   ;Path to bat to execute. Qual with path!
batYNstr    db 2,1," ",CR           ;String for buffered Y/N input
ifFlg       db 0                    ;Flags for IF
ifReset     equ 0                   ;Value to reset the flags
ifNot       equ 1                   ;Set if NOT encountered
ifCond      equ 2                   ;Set if condition encountered              

;Structs and strings
cmdFcb      db 10h dup (0) ;Internal "fcb" for parsing the command name
cmdFFBlock  db ffBlock_size dup (0) ;Internal Find First Block to use as default DTA

launchBlock db execProg_size dup (0)

;Use the below figure for the buffer "length" (byte 0) as this will allow 
; for us to type 127 characters plus a mandatory terminating 128th <CR>. 
; This will always be ok for copying to the PSP as on the PSP we have space for
; 127 chars. If we type 128 chars with terminating <CR>, the command name must be 
; at least 1 character long. The tail is formed of the remaining chars, so there
; will be at least 127 chars left. Thus we always have enough space.
inLen   equ 128 
batCpyBuffer:   ;Ptr to the buffer to drop the processed line into
inBuffer    db cmdBufferL dup (0)   ;Original input from user! 128 chars max!
batInBuffer:    ;Ptr to the buffer for batch input
cpyBuffer   db cmdBufferL dup (0)   ;Copied input for processing
cmdBuffer   db cmdBufferL dup (0)   ;Buffer with the command pipeline
cmdPathSpec db fileSpecZL dup (0)   ;Space for full path to a ext cmd
cmdName     db cmdNameL dup (0)     ;Cmd name prefixed by length 

rdrInFilespec   db fileSpecZL dup (0)   ;Space for the redir in filespec
rdrOutFilespec  db fileSpecZL dup (0)   ;Space for the redir out filespec

;Once we are done with a pathname, we override the first byte with a NULL.
pipe1Filespec   db fileSpecZL dup (0)   ;Space for the pipe file filespec
pipe2Filespec   db fileSpecZL dup (0)   ;Space for the pipe file filespec

newPipe dq 0    ;Pointer to the new pathspec (STDOUT)
oldPipe dq 0    ;Pointer to the old pathspec (STDIN)

;Main scratch buffer for forming paths! Needs to be large enough to splice
; a really long invalid DOS path for PATH to work properly.
searchSpec  db 2*cmdBufferL dup (0)   

;Internal Function vars
;Dir Vars
dirFlags    db 0    ;Dir Flags.     Bit[0] set => /W or /w specified
;                                   Bit[1] set => /P or /p specified
;                                   Bit[2] set => A file/path specified
dirLineCtr  db 0    ;Counter to keep track of which line we printed (0-23)
dirFileCtr  dd 0    ;Used in /W mode, rollover after 5
dirDrv      db 0    ;0 based drive number to use
dirSrchDir  db cmdBufferL dup (0)   ;Search directory 
dirSrchFCB  db 10h dup ("?")    ;We copy the search pattern here
dirWideType equ 1
dirPageType equ 2
dirFileType equ 4
;Volume Vars
volFcb:
    istruc exFcb
    at exFcb.extSig,    db -1   ;Indicate extended FCB
    at exFcb.attribute, db dirVolumeID
    at exFcb.driveNum,  db 0    ;Current drive
    at exFcb.filename,  db "????????"
    at exFcb.fileext,   db "???"
    at exFcb.curBlock,  dd 0
    iend 

;Time/Date vars
td1 db 0    ;Minutes/Year
td2 db 0    ;Hours/Zero
td3 db 0    ;Hundredths/Day
td4 db 0    ;Seconds/Month

;Rename/Copy/Delete Buffers
delPath:
comspecDir:    ;Used to store the directory passed during startup
srcSpec     db cmdBufferL dup (0)
destSpec    db cmdBufferL dup (0)
srcPtr      dq 0    ;Where to copy pattern to in src path
destPtr     dq 0    ;Where to copy pattern to in dest path
renName     db 11 dup (" ") ;Build a name pattern here in FCB format
;Copy vars
verifyFlg   db 0    ;Set if verify on before copy
sourceHdl   dw -1
destHdl     dw -1
srcHdlInfo  dw 0    ;Save the hdl device info (bit 7 Set -> Char dev)
cpBufPtr    dq 0    ;Ptr to the xfr arena
wCpBufSz    dw 0    ;Copy Buffer size, max 4096 bytes
dCpCnt      dd 0    ;Number of files we have copied
bCpFlg      db 0    ;Copy state flag

ascSrc      equ 1   ;Set if ascii copy for this source file.
binSrc      equ 2   ;Set if last encountered src file flag was binary
ascDes      equ 4   ;Set if add ^Z at end of file. Clear if not!
wcSrc       equ 8   ;Set if wc's in source pattern. Display source file names.
oneDest     equ 10h ;Single destination, not dir 
mod1Cpy     equ 20h ;Set if copying files to new dir with same names
mod2Cpy     equ 40h ;Set if copying files with new names
mod3Cpy     equ 80h ;Set if dflt cat ASCII files to a single destination

;Environment manipulation vars
envVarSz        dw 0    ;Env var size
envVarNamSz     dw 0    ;Env var "name="" length