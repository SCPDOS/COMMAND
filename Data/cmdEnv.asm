    
masterEnv:  ;Yeet this is this is a child command processor, length 168 chars!
;Note this environment DOES NOT contain the full path to the program being launched
    db "PATH=",0
    db "COMSPEC="
.cspec: 
    db "_:\COMMAND.COM", 0
    db (165 - ($ - masterEnv)) dup (" ")    ;Pad the environment with spaces
    dw 0    ;End of environment marker
    db 0    ;End of "program path section"
