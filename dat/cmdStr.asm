;COMMAND.COM Messages and strings
crlf    db  CR,LF,"$"
crlf2   db  CR,LF,CR,LF,"$"
badBat  db  CR,LF,"Batch file missing",CR,LF,"$"    ;Used in BAT
needBat db  CR,LF,"Insert disk with batch file"     ;Used in BAT
        db  CR,LF,"and press any key when ready",CR,LF,"$"
batFail db  CR,LF,"Terminate batch job (Y/N)? $"
badCmd  db  "Bad command or file name",CR,LF,"$"
dupName db  "Duplicate file name or "
fnfMsg  db  "File not found",CR,LF,"$"
noSpace db  "Insufficient disk space",CR,LF,"$"     ;Disk full.
noEnvSpace  db  "Out of environment space",CR,LF,"$"
fulRootDir  db  "File creation error",CR,LF,"$"
noSelfCopy  db  "File cannot be copied onto itself",CR,LF,"$"
filLostErr  db  "Content of destination lost before copy",CR,LF,"$"
;Copy end message. First write # files copied then this message 
copyOk  db  " File(s) copied",CR,LF, "$"
cpNoMem db  "Not enough memory for COPY",CR,LF,"$"      ;Never should happen
;Dir end1 msg. First write # files in dir then this
dirOk   db  " File(s) $"
;Dir end2 msg. First write # bytes free then this message
bytesOk db  " bytes free",CR,LF, "$"
dirLbl  db  " <DIR>  $"

badDrv  db  "Invalid drive specification",CR,LF,"$"
pauseMes     db  "Strike a key when ready . . . $"
badParm db  "Invalid parameter",CR,LF,"$"

dayName db  "SunMonTueWedThuFriSat"
badDate db  CR,LF,"Invalid date$"
curDate db  "Current date is $"
newDate db  CR,LF,"Enter new date $"
usDate  db  "(mm-dd-yy): $"
ukDate  db  "(dd-mm-yy): $"
jpDate  db  "(yy-mm-dd): $"
badTime db  CR,LF,"Invalid time$"
curTime db  "Current time is $"
newTime db  CR,LF,"Enter new time: $"

ynMes   db  "Are you sure (Y/N)? $"

dosVer  db " SCP/DOS Version $"

volMes  db " Volume in drive $"
volOk   db " is $"
volNo   db " has no label$"

badDir  db "Invalid Directory", CR,LF, "$"
badMD   db "Unable to create directory",CR,LF,"$"
badRD   db "Invalid path, not directory,",CR,LF
        db "or directory not empty",CR,LF,"$"
dirMain db " Directory of  $"
noPath  db "No Path $"
accDenMsg  db "Access denied",CR,LF,"$"
badDrvMsg db "Current drive is no longer valid$"
;badDrvSrch  db "Invalid drive in search path",CR,LF,"$"        ;MAY REMOVE
badDev  db "Invalid device",CR,LF,"$"
badLbl  db "Label not found",CR,LF,"$"  ;Used in BAT
syntaxErr   db "Syntax error",CR,LF,"$"
forNest db CR,"FOR cannot be nested",CR,LF,"$"    ;Used in BAT
forStr  db "FOR",0
pipeErr db "Intermediate file error during pipe",CR,LF,"$"
binDevErr   db "Cannot do binary reads from a device",CR,LF,"$"
offMes  db "off",CR,LF,"$"
offStr  db "OFF",0
onMes   db "on",CR,LF,"$"
onStr   db "ON",0
breakIs db "BREAK is $"
verifyIs    db "VERIFY is $"
echoIs  db "ECHO is $"  
badSpec db "Invalid path or file name",CR,LF,"$"
badArgs db "Invalid number of parameters",CR,LF,"$"
devWriteErr db "Error writing to device"
backSpace   db BSP," ",BSP,NUL
noMemMsg    db "Program too big to fit in memory",CR,LF,"$"
notString   db "NOT",0
errlvlStr   db "ERRORLEVEL",0
existStr    db "EXIST",0
inStr       db "IN",0
doStr       db "DO",0
listOpenStr db "(",0
listClosStr db ")",0

memSys  db CR,LF,"              Total system memory: $"
memDOS  db CR,LF,"               Memory used by DOS: $"
memApp  db CR,LF,"      Memory used by applications: $"
memHole db CR,LF,"      Memory reserved by hardware: $"
memFree db CR,LF,"                      Memory free: $"
memByte db " bytes$"
memBad0 db CR,LF,"Could Not Assertain DOS Entry Point$"
memBad1 db CR,LF,"Memory Allocation Error$"
memBad2 db CR,LF,"Memory Error$"
memBad3 db CR,LF,"System halted$"

pipeErrMsg  db "Unable to create pipe",CR,LF
pipeErrMsgL equ $ - pipeErrMsg
redirErrMsg db "Redirection error",CR,LF
redirErrMsgL   equ $ - redirErrMsg

ansiCls  db ESC,"[2J" ;ANSI CLS sequence, 4 chars long
fourSpc  db SPC
threeSpc db SPC, SPC, SPC,"$"

badOnOff db "Must specify ON or OFF",CR,LF,"$"

promptEVar  db "PROMPT=",CR     ;Must be CR terminated!
extStr  db "COMEXEBAT"  ;Used for extension searches
;If anything goes wrong with piping or redirecting just close first two 
; handles and reopen this device. Defaults to CON
devName db "CON", 6 dup (0) ;8 chars + space for null terminator
autoSpec    db "_:\AUTOEXEC.BAT",0
autoSpecL equ $ - autoSpec
pathEVar    db "PATH=",0
comspecEVar db "COMSPEC=",0
cspec   db "COMMAND.COM", 0

;Int 24h strings
errMsgPtrTbl:
    dw errorMsgTbl.0 - errMsgPtrTbl
    dw errorMsgTbl.1 - errMsgPtrTbl
    dw errorMsgTbl.2 - errMsgPtrTbl
    dw errorMsgTbl.3 - errMsgPtrTbl
    dw errorMsgTbl.4 - errMsgPtrTbl
    dw errorMsgTbl.5 - errMsgPtrTbl
    dw errorMsgTbl.6 - errMsgPtrTbl
    dw errorMsgTbl.7 - errMsgPtrTbl
    dw errorMsgTbl.8 - errMsgPtrTbl
    dw errorMsgTbl.9 - errMsgPtrTbl
    dw errorMsgTbl.A - errMsgPtrTbl
    dw errorMsgTbl.B - errMsgPtrTbl
    dw errorMsgTbl.C - errMsgPtrTbl
    dw errorMsgTbl.D - errMsgPtrTbl
    dw errorMsgTbl.E - errMsgPtrTbl
    dw errorMsgTbl.F - errMsgPtrTbl
    dw errorMsgTbl.10 - errMsgPtrTbl
    dw errorMsgTbl.11 - errMsgPtrTbl
errorMsgTbl:
.0: db "Write Proctect $"       ;Driver Error 0 / DOS Error 013h
.1: db "Unknown Unit $"         ;Driver Error 1 / DOS Error 014h
.2: db "Not Ready $"            ;Driver Error 2 / DOS Error 015h
.3: db "Unknown Command $"      ;Driver Error 3 / DOS Error 016h
.4: db "Data $"                 ;Driver Error 4 / DOS Error 017h
.5: db "Bad Request $"          ;Driver Error 5 / DOS Error 018h
.6: db "Seek $"                 ;Driver Error 6 / DOS Error 019h
.7: db "Unknown Media $"        ;Driver Error 7 / DOS Error 01Ah
.8: db "Sector Not Found $"     ;Driver Error 8 / DOS Error 01Bh
.9: db "Out Of Paper $"         ;Driver Error 9 / DOS Error 01Ch
.A: db "Write Fault $"          ;Driver Error A / DOS Error 01Dh
.B: db "Read Fault $"           ;Driver Error B / DOS Error 01Eh
.C: db "General Failure $"      ;Driver Error C / DOS Error 01Fh
.D: db "Sharing violation $"    ;SHARE Error / DOS Error 020h
.E: db "Lock violation $"       ;SHARE Error / DOS Error 021h
;Driver Error F / DOS Error 022h
.F: db "Invalid Disk Change",CR,LF,"Please Insert disk "    
.FVol:  db 11 dup (" ")         ; Volume name for disk. Setup before print
        db CR,LF,"$"
;Next two strings dont print any more information, such as reading/writing etc
.10:    db "FCB unavailable $"      ;FCB Error / DOS Error 023h - RESERVED
.11:    db "Sharing buffer error $" ;SHARE Error / Error 024h

;Anything above this is a generic network error.
genNetErr   db "General Network Error $"    ;All NET errors codes [32h, 58h]

;Rest of the Int 24h error messages go here
drvMsg      db "drive $"
readMsg     db "error reading $"
writeMsg    db "error writing $"
abortMsg    db "Abort$" 
ignoreMsg   db "Ignore$"
retryMsg    db "Retry$"
failMsg     db "Fail$"
betweenMsg  db ", $"
endMsg      db "? $"
i24Resp     db "IRAF"   ;Abort Retry Ignore Fail