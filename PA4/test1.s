li  $v0, 4 # system call code for print_str
.word 0x00636261, 0x00000000
la $a0, 4194308 # address of string to print 
#lui $s0, 0x40 # 0x1000 means 1000 base 16 or 4096 base 10
#lw $a0, 0x4($s0) # 0x10000000 + 0x8000 = 0x10008000
syscall # print the string
li $v0, 1 # system call code for print_int
li $a0, 5 # integer to print 
syscall # print it