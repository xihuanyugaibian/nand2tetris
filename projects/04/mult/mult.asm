// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Put your code here.
//已有的计算规范没有乘法，用加法来计算乘法。5*6=5+5+5+5+5+5=6+6+6+6+6
//这里计算R0*R1采用R0个R1相加。
@R3 //R3用于保存R1相加的个数，初始为0
M=0 //初始化R3为0，并存入R3
@R2 //将R2放入寄存器准备初始化
M=0 //初始化R2为0并存入R2
(LOOP) //标签符号：循环开始的地方
@R3 //判断是否满足循环条件，
D=M
@R0
D=D-M   //应该加的次数和已经相加次数的差
@END
D;JGE   //差=0表示已经加够次数了，如果差>=0 就可以跳出循环。如果<0,就继续执行。
@R1
D=M //将R1的值放入D寄存器
@R2
M=D+M   //用R2的值加一次R1，每循环一次就加一次。
@R3
M=M+1   //相加的次数+1，用于下次循环判断是否继续
@LOOP
0;JMP
(END)
0;JMP
