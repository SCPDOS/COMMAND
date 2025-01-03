#!/bin/sh
#Build COMMAND.COM
command:
	nasm command.asm -o ./bin/COMMAND.COM -f bin -l ./lst/command.lst -O0v

# Build, mount, copy and unmount
all:
	nasm command.asm -o ./bin/COMMAND.COM -f bin -l ./lst/command.lst -O0v
	~/mntdos
	sudo cp ./bin/COMMAND.COM /mnt/DOSMNT/COMMAND.COM
	~/umntdos