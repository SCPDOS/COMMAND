;Function dispatch table
functionTable:
;Use Pascal strings with each row of hte table having three columns:
; Col 1, BYTE, Length of command
; Col 2, String, String representing the user input
; Col 3, WORD, Offset from the startLbl into COMMAND.COM of the function
    db 3, "DIR"
    dw dir - startLbl

    db 2, "CD"
    dw chdir - startLbl

    db 5, "CHDIR"
    dw chdir - startLbl

    db 2, "MD"
    dw mkdir - startLbl

    db 5, "MKDIR"
    dw mkdir - startLbl

    db 2, "RD"
    dw rmdir - startLbl

    db 5, "RMDIR"
    dw rmdir - startLbl

    db 3, "DEL"
    dw erase - startLbl

    db 5, "ERASE"
    dw erase - startLbl

    db 4, "DATE"
    dw date - startLbl

    db 4, "TIME"
    dw time - startLbl

    db 4, "COPY"
    dw copy - startLbl

    db 4, "CTTY"
    dw ctty - startLbl

    db 3, "CLS"
    dw cls - startLbl

    db 5, "BREAK"
    dw break - startLbl

    db 6, "VERIFY"
    dw verify - startLbl

    db 6, "RENAME"
    dw rename - startLbl

    db 3, "REN"
    dw rename - startLbl
    
    db 8, "TRUENAME"
    dw truename - startLbl

    db 3, "VER"
    dw version - startLbl

    db 3, "VOL"
    dw volume - startLbl

    db 3, "MEM"
    dw memory - startLbl

    db 4, "EXIT"
    dw exit - startLbl

    db 4, "TYPE"
    dw type - startLbl

    db 4, "PATH"
    dw pathEdit - startLbl

    db 3, "SET"
    dw set - startLbl

    db 6, "PROMPT"
    dw prompt - startLbl

;BATCH ORIENTED COMMANDS HERE
    db 4, "ECHO"
    dw echo - startLbl

    db 5, "PAUSE"
    dw pauza - startLbl

    db 3, "REM"
    dw remark - startLbl
    db -1   ;End of table
