    
masterEnv:
;This is copied into an MCB to be used as the environment!
comspecEvar:
    db "COMSPEC="
.cspec: 
    db "_:\COMMAND.COM", 0  ;Part of the comspec!
pathEVar:
    db "PATH=",0,0  ;End of default environment strings
menv_len equ $ - masterEnv
;Master environment doesnt contain the "program name string"