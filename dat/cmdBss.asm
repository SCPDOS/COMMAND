
;Moved most strings and initialised 0 variables here to reduce disk image size
returnCode  dw ?    ;Return Code from a child process
int2Epsp    dq ?    ;
int2Ersp    dq ?    ;Save the far Int 2E entry stack pointer!
int2Edta    dq ?    ;
;=============================================================
statFlg1    db ?    ;Flags 1 for the command interpreter
;=============================================================
permaShell  equ 1   ;Up if we are a permanent command interpreter
inCtrlC     equ 2   ;Up if we are processing int 23h
inSingle    equ 4   ;Up if processing a single command (/C mode)
inBatch     equ 8   ;Up if processing a batch file
batchEOF    equ 10h ;Set to indicate we have reached the end of the batch file
inCritical  equ 20h ;Up if processing a command.
inLdrDT     equ 40h ;Up if in the Date / Time part of Loader
inLdr       equ 80h ;Up if in loader
;Batch state variables.
bbPtr       dq ?                    ;Ptr to the batch block
batFile     db fileSpecZL dup (?)   ;Path to bat to execute. Qual with 
ifFlg       db ?                    ;Flags for IF
ifReset     equ 0                   ;Value to reset the flags
ifNot       equ 1                   ;Set if NOT encountered
ifCond      equ 2                   ;Set if condition encountered 
callFlg     db ?                    ;Set to -1 in a call!
;FOR state variables
forFlg      db ?                    ;For flag. Set if in a forloop
pForBlk     dq ?                    ;Ptr to the for block

;Structs and strings
ctryData    db countryStruc_size dup (?)    ;Length of the country table
currDirStr  db fullDirPathZL dup (?)        ;Current Directory String
cmdFcb      db 10h dup (?) ;Internal "fcb" for parsing the command name
cmdFFBlock  db ffBlock_size dup (?) ;Internal Find First Block to use as default DTA

launchBlock db execProg_size dup (?)

;Use the below figure for the buffer "length" (byte 0) as this will allow 
; for us to type 127 characters plus a mandatory terminating 128th <CR>. 
; This will always be ok for copying to the PSP as on the PSP we have space for
; 127 chars. If we type 128 chars with terminating <CR>, the command name must be 
; at least 1 character long. The tail is formed of the remaining chars, so there
; will be at least 127 chars left. Thus we always have enough space.
inLen   equ 128 
inBuffer    db cmdBufferL dup (?)   ;Original input from user! 128 chars max!
batInBuffer:    ;Ptr to the buffer for batch input
cLineBuffer db cmdBufferL dup (?)   ;Copied input for processing
cmdBuffer   db cmdBufferL dup (?)   ;Buffer with the command pipeline
cmdPathSpec db fileSpecZL dup (?)   ;Space for full path to a ext cmd
cmdName     db cmdNameL dup (?)     ;Cmd name prefixed by length 

rdrInFilespec   db fileSpecZL dup (?)   ;Space for the redir in filespec
rdrOutFilespec  db fileSpecZL dup (?)   ;Space for the redir out filespec

;Once we are done with a pathname, we override the first byte with a NULL.
pipe1Filespec   db fileSpecZL dup (?)   ;Space for the pipe file filespec
pipe2Filespec   db fileSpecZL dup (?)   ;Space for the pipe file filespec

newPipe dq ?    ;Pointer to the new pathspec (STDOUT)
oldPipe dq ?    ;Pointer to the old pathspec (STDIN)

;Main scratch buffer for forming paths! Needs to be large enough to splice
; a really long invalid DOS path for PATH to work properly.
searchSpec  db 2*cmdBufferL dup (?)   

;Internal Function vars
;Dir Vars
dirFlags    db ?    ;Dir Flags.     Bit[0] set => /W or /w specified
;                                   Bit[1] set => /P or /p specified
;                                   Bit[2] set => A file/path specified
dirLineCtr  db ?    ;Counter to keep track of which line we printed (0-23)
dirFileCtr  dd ?    ;Used in /W mode, rollover after 5
dirDrv      db ?    ;0 based drive number to use
dirSrchDir  db cmdBufferL dup (?)   ;Search directory 
dirSrchFCB  db 10h dup (?)    ;We copy the search pattern here, inited in dir
dirWideType equ 1
dirPageType equ 2
dirFileType equ 4

;Time/Date vars
td1 db ?    ;Minutes/Year
td2 db ?    ;Hours/Zero
td3 db ?    ;Hundredths/Day
td4 db ?    ;Seconds/Month

;Rename/Copy/Delete Buffers
delPath:
comspecDir:    ;Used to store the directory passed during startup
srcSpec     db cmdBufferL dup (?)
destSpec    db cmdBufferL dup (?)
srcPtr      dq ?    ;Where to copy pattern to in src path
destPtr     dq ?    ;Where to copy pattern to in dest path
renName     db 11 dup (?) ;Build a name pattern here in FCB format
;Copy vars
pLastFspec  dq ?    ;Ptr to first char past last + in source of Mode 3 copy
pNextFspec  dq ?    ;Ptr to first char past next + in source of Mode 3 copy
verifyFlg   db ?    ;Set if verify on before copy
sourceHdl   dw ?    ;These get set to -1 on entry to copy
destHdl     dw ?
srcHdlInfo  dw ?    ;Save the hdl device info (bit 7 Set -> Char dev)
cpBufPtr    dq ?    ;Ptr to the xfr arena
wCpBufSz    dw ?    ;Copy Buffer size, max 4096 bytes
dCpCnt      dd ?    ;Number of files we have copied
bCpFlg      db ?    ;Copy state flag   
ascSrc      equ 1   ;Set if ascii copy for this source file.
binSrc      equ 2   ;Set if last encountered src file flag was binary
ascDes      equ 4   ;Set if add ^Z at end of file. Clear if not!
wcSrc       equ 8   ;Set if wc's in source pattern. Display source file names.
oneDest     equ 10h ;Single destination, not dir 
mod1Cpy     equ 20h ;Set if copying files to new dir with same names
mod2Cpy     equ 40h ;Set if copying files with new names (unused)
mod3Cpy     equ 80h ;Set if dflt cat ASCII files to a single destination

;Environment manipulation vars
envVarSz        dw ?    ;Env var size
envVarNamSz     dw ?    ;Env var "name="" length
