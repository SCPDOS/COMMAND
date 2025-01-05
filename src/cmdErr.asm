;This file contains the error printing routines
;Common Error Messages, jumped to to return from
badDiskFull:
;This is a different error return, as this closes handles and prints
; disk full error and resets the command line!
    lea rdx, noSpace
    lea rcx, pipeErr
    test byte [pipeFlag], -1    ;Is the flag set
    jz short badCmn ;If its just disk full, dont go through pipe clean
    cmovnz rdx, rcx ;Swap error messages if pipe flag is on
    call badCmn     ;Print the string
    jmp redirPipeFailureCommon.noPrint  ;Now close pipes and fully reset!
badForError:    
    call forFree        ;Free all FOR variables
    lea rdx, forNest
    jmp short badCmn    
badNoMemError:
    lea rdx, noMemMsg
    jmp short badCmn
badSyntaxError:
    lea rdx, syntaxErr
    jmp short badCmn
badEnvSpaceError:
    lea rdx, noEnvSpace
    jmp short badCmn
badAccError:
    lea rdx, accDenMsg
    jmp short badCmn
badParamError:
    lea rdx, badParm
    jmp short badCmn
badDriveError:
    lea rdx, badDrv
    jmp short badCmn
badArgError:
    lea rdx, badArgs
    jmp short badCmn
badFileError:
    lea rdx, badSpec
    jmp short badCmn
badDupFnf:
;Hybrid error message
    lea rdx, dupName
    jmp short badCmn
badFnf:
    lea rdx, fnfMsg
    jmp short badCmn
badDirError:
    lea rdx, badDir
badCmn:
    jmp printString
badCmdError:
    lea rdx, badCmd
    jmp short badCmn