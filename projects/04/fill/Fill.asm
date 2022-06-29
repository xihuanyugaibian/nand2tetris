// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed.
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.
//读题 有按键按下时，屏幕会主键变黑，没有按键时，屏幕会主键清屏。是慢慢的一个一个来的
//代码实现逻辑，如果有按钮按下，从开始位置逐渐黑屏，如果没有按钮按下，从开始主键清屏。
//每一次按下都会使清屏位置初始化，每一次松开都会使黑屏位置初始化。相当于每次的开始都是从起始位置。
//这样逻辑简单，但是效率底。也可以清屏从最后一个黑屏往前，黑屏从最后一个黑屏往后。并且没有设置结束位置。
@SCREEN
D=A
@R0 //BLACK当前位置
M=D //初始化为屏幕开始位置
@R1 //CLEAN当前位置
M=D //初始化为屏幕开始位置
(LOOP)
@KBD
D=M //获取键盘对应的内存中的数据
@BLACK
D;JNE   //有按键是，数据不为0.没有案件时数据为0.不等于0表示有按键按下，需要黑屏，跳转到黑屏执行位置
@CLEAN
0;JMP

(BLACK) //黑屏逻辑执行
@SCREEN
D=A
@R1
M=D //执行黑屏逻辑，表示有按钮按下，就把清屏的当前位置初始化。如果松开按键的话，就从开始位置进行清屏。
@R0
A=M
M=1
@R0
M=M+1
@LOOP
0;JMP

(CLEAN) //清屏逻辑执行
@SCREEN
D=A
@R0
M=D
@R1
A=M
M=0
@R1
M=M+1
@LOOP
0;JMP
