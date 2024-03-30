;Static Data Area for COMMAND.COM    
startLbl:   ;Start symbol, this is the entry point
    jmp cmdLdr
stackTop    dq 0    ;Pointer to the top of the stack as allocated by DOS
returnCode  dw 0    ;Return Code from a child process
pspPtr      dq 0    ;Internal pointer to the task PSP
realParent  dq -1   ;Only the first Copy of COMMAND.COM sets itself here
sysVars     dq 0    ;Ptr to DOS sysvars
numHdls     dw 20   ;Get number of handles permitted
promptPtr   dw -1   ;Offset From Environemnt Start to prompt String. -1 => dflt 
pathSep     db "\"  ;Default path sep
switchChar  db "/"  ;Default switch char
permaSwitch db 0    ;If -1, EXIT should just return. If 0, EXIT returns
parentInt22 dq 0    ;Stores the parent values to restore upon exiting if it can
;Static strings, not used in command line parsing
ctryData    db countryStruc_size dup (0)  ;Length of the country table
currDirStr  db fullDirPathZL dup (0) ;Current Directory String


cmdLineStatePtr:
cmdStartPtr dq 0    ;Ptr to the first char for this command (may be a space)
cmdEndPtr   dq 0    ;Ptr to the terminating char for this command (0Dh or |)
pipeFlag    db 0    ;If set, we fired up a pipe for this command line
pipeSTDIN   dw -1   ;The handle to replace STDIN with once all piping complete
pipeSTDOUT  dw -1   ;The handle to replace STDOUT with once all piping complete

cmdStatePtr:   ;Symbol to use for clearing command state variables
;These variables are valid for a SINGLE command in a command line
;Next two bytes, if set to -1, flags error
redirIn     db 0    ;If set, we are redirecting input from a file
redirOut    db 0    ;If 1, we are redirecting output to a file, destructively
;                    If 2, we are redirecting output to a file, by appending
redirSTDIN  dw -1   ;The handle to replace STDIN with once redir complete
redirSTDOUT dw -1   ;The handle to replace STDOUT with once all redir complete

arg1Flg     db 0    ;Set if there was a first argument
arg1Off     db 0    ;Offset into cmdBuffer to the argument
arg1FCBret  db 0    ;AL on return from parse filename for argument 1

arg2Flg     db 0    ;Set if there was a second argument
arg2Off     db 0    ;Offset into cmdBuffer to the argument
arg2FCBret  db 0    ;AL on return from parse filename for argument 2

cmdStateL equ $ - cmdStatePtr
cmdLineStateL equ $ - cmdLineStatePtr

;Structs and strings

cmdFcb      db 10h dup (0) ;Internal "fcb" for parsing the command name
cmdFFBlock  db ffBlock_size dup (0) ;Internal Find First Block to use as default DTA

launchBlock db execProg_size dup (0)
cmdTail     db cmdBufferL dup (0)   ;This is the tail that is passed to EXEC

inBuffer    db cmdBufferL dup (0)  ;Add one to add space for terminating CR
cmdBuffer   db cmdBufferL dup (0)  ;This is the to copy input to when processing
cmdPathSpec db fileSpecZL dup (0)  ;Space for full path to a external command

fcbCmdSpec  db fcbNameL dup (0) ;Used to make a FCB style name for the file
cmdSpec     db fileNameZL dup (0)   ;ASCIIZ command spec for the command name
cmdName     db cmdNameL dup (0) ;Command name string prefixed by length of word

rdrInFilespec   db fileSpecZL dup (0)   ;Space for the redir in filespec
rdrOutFilespec  db fileSpecZL dup (0)   ;Space for the redir out filespec

;Once we are done with a pathname, we override the first byte with a NULL.
pipe1Filespec   db fileSpecZL dup (0)   ;Space for the pipe file filespec
pipe2Filespec   db fileSpecZL dup (0)   ;Space for the pipe file filespec

newPipe dq 0    ;Pointer to the new pathspec (STDOUT)
oldPipe dq 0    ;Pointer to the old pathspec (STDIN)

searchSpec  db cmdBufferL dup (0)   ;Contains the pathspec for the search file
;The above is larger than is needed/supported by DOS to allow for 
; users overtyping

;Internal Function vars
;Dir Vars
dirFlags    db 0    ;Dir Flags.     Bit[0] set => /W or /w specified
;                                   Bit[1] set => /P or /p specified
;                                   Bit[2] set => A file/path specified
dirLineCtr  db 0    ;Counter to keep track of which line we printed (0-23)
dirFileCtr  db 0    ;Used in /W mode, rollover after 5
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

;Rename/Copy Buffers
sourcePath  db cmdBufferL dup (0)
destPath    db cmdBufferL dup (0)
;Copy Handles
sourceHdl   dw -1
destHdl     dw -1
srcHdlInfo  dw 0 ;Used to save the handle device info (bit 7 Set -> Char dev)

copyBuffer  db 128 dup (0)  ;Copy up to 128 bytes at a time

bpActive db 0   ;When not zero, breakpoints are on!