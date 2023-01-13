#!/bin/sh
#Build COMMAND.COM
command:
	nasm command.asm -o ./Binaries/COMMAND.COM -f bin -l ./Listings/command.lst -O0v
