#Build COMMAND.COM
command:
	nasm ./Utils/COMMAND/command.asm -o ./Utils/COMMAND/Binaries/COMMAND.COM -f bin -l ./Utils/COMMAND/Listings/command.lst -O0v
