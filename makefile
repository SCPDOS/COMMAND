#!/bin/sh
#Build COMMAND.COM
command:
	nasm command.asm -o ./Binaries/COMMAND.COM -f bin -l ./Listings/command.lst -O0v

# Build, mount, copy and unmount
all:
	nasm command.asm -o ./Binaries/COMMAND.COM -f bin -l ./Listings/command.lst -O0v
	~/mntdos
	sudo cp ./Binaries/COMMAND.COM /mnt/DOSMNT/COMMAND.COM
	~/umntdos