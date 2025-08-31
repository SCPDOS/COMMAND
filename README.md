# COMMAND
COMMAND.COM

A note on launching COMMAND.COM

If an application wishes to launch (EXEC) a copy of COMMAND.COM, the application must pass to DOS, a pointer to a 128 buffer that adheres to the following standard:

cmdTail[0] = Count byte, n, max 126

cmdTail[1-n] = Command line, String of length n

cmdTail[n+1] = Terminating <CR> char

If the count byte is set to a value greater than 126, any char in the buffer after the 126th character is ignored by COMMAND.COM.
