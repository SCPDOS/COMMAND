;FCB related structures

;Note: If record size > 64, high byte of randRecrd is ignored.
;This field also used for random read/write ops to find the right record
struc fcb
    .driveNum   db ?  ;0 based drive number
    .filename   dq ?
    .fileext    db 3 dup (?)
    .curBlock   dw ?  ;Current block number relative to 0
;One block consists of 128 records, of size denoted in the below
    .recordSize dw ?  ;Record size, default 80h
    .fileSize   dd ?  ;File size, in bytes
    .fileDate   dw ?  ;Standard date format
    .fileTime   dw ?  ;Standard time format
    ;----------------------------------------
    ;Internal Records
    .sftNdx     db ?  ;Internal, SFT ndx number
    .fcbMode    db ?  ;Internal byte, to indicate format of special field
    .special    db 6 dup (?)  ;For use with below internal structures
    ;----------------------------------------
    .recordNum  db ?  ;Current record in block
    .randRecrd  dd ?  ;Relative record number, relative to start of file
endstruc

struc exFcb
    .extSig     db ?  ;If the first byte is -1, signals an exFCB
    .reserved   db 5 dup (?)  
    .attribute  db ?  ;File attribute byte
    .driveNum   db ?  ;1 based drive number 
    .filename   dq ?
    .fileext    db 3 dup (?)
    .curBlock   dw ?  ;Current block number relative to 0
;One block consists of 128 records, of size denoted in the below
    .recordSize dw ?  ;Record size, default 80h
    .fileSize   dd ?  ;File size, in bytes
    .fileDate   dw ?  ;Standard date format
    .fileTime   dw ?  ;Standard time format
    ;----------------------------------------
    ;Internal Records
    .sftNdx     db ?  ;Internal, SFT ndx number
    .fcbMode    db ?  ;Internal byte, to indicate format of special field
    .special    db 6 dup (?)  ;For use with below internal structures
    ;----------------------------------------
    .recordNum  db ?  ;Current record in block
    .randRecrd  dd ?  ;Relative record number, relative to start of file
endstruc

struc renameFcb
;Used for accessing the names and drive letter for renaming
;The fcb ptr must be pointing to the drivenumber even if the passed fcb was ext
    .driveNum   db ?
    .filename   dq ?
    .fileext    db 3 dup (?)
    .reserved   dd ?  ;Alignment bytes, cleared by AH=29h
    .resDrvNum  db ?  ;Always zero, used by AH=29h for the drive letter
    .newName    dq ?
    .newExt     db 3 dup (?)
    .reserved2  dd ?  ;Alignment bytes, cleared by AH=29h
endstruc

struc exRenFcb
;Used for accessing the names and drive letter for renaming
;The fcb ptr must be pointing to the drivenumber even if the passed fcb was ext
    .extSig     db ?  ;If the first byte is -1, signals an exFCB
    .res0       db 5 dup (?)  
    .attribute  db ?  ;File attribute byte
    .driveNum   db ?
    .filename   dq ?
    .fileext    db 3 dup (?)
    .reserved   dd ?  ;Alignment bytes, cleared by AH=29h
    .resDrvNum  db ?  ;Always zero, used by AH=29h for the drive letter
    .newName    dq ?
    .newExt     db 3 dup (?)
    .reserved2  dd ?  ;Alignment bytes, cleared by AH=29h
endstruc

;FCBmode equates:
;Bit 7 = Set => Share loaded, Clear => Share not loaded
;Bit 6 = Set => Char device, Clear => Disk file
;Bits [5,0] = Low 6 bits of device attribute word 
fcbShareOn  equ 80h
fcbCharDev  equ 40h
fcbLocal    equ 40h

;Structures of the special 6 bytes

;Used when share is NOT loaded (fcbMode = 0xxxxxxxb)
struc noShare   
    .attribWord db ?  ;(lowbyte DevAttrWord && 0Ch) || open mode
    .startClust dw ?  ;Starting cluster of file (too small for FAT32)
    .dirSect    dw ?  ;Sector containing dir entry (too small for FAT32)
    .dirEntry   db ?  ;Number of 32 byte directory entry IN sector
endstruc

;Used when the volume is local and share is loaded (fcbMode = 11xxxxxxb)
struc shareLocal    
    .strtClust  dw ?  ;Starting cluster on disk of file (too small for FAT32)
    .shareSig   dw ?  ;Share record number for this file (to find record)
    .fileAttr   dw ?  ;File attribute (accessed as a byte)
endstruc

;Used when the volume is remote and share is loaded (fcbMode = 10xxxxxxb)
struc shareNet
;All three fields too small for FAT32
    .dirSect    dw ?  ;Sector number containing dir entry
    .relClust   dw ?  ;File relative cluster of last cluster accessed
    .absClust   dw ?  ;Disk relative cluster of last cluster accessed
endstruc