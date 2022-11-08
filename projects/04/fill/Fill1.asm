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
//代码实现逻辑，如果有按钮按下，从开始位置逐渐黑屏，如果没有按钮按下，从变黑的位置往前清除。
//屏幕一行有32列，每列中又有16个元素，相当于每列又划分了16份，每份对应一位二进制，1表示黑，0表示clean。
@SCREEN
D=A
M[R0]=D      //RO保存变黑的开始位置
(LOOP)
D=M[KBD]     //获取键盘对应的内存中的数据
@BLACK
D;JNE        //有按键是，数据不为0.没有按键时数据为0.不等于0表示有按键按下，需要黑屏，跳转到黑屏执行位置
@CLEAN
0;JMP

(BLACK)      //黑屏逻辑执行
AD=M[R0]
M=-1         //从R0处开始变黑,每行分成了32列,每列其实16个像素,1是变黑一个像素点,-1是全变黑.
D=D+1
M[R0]=D      //更新R0为下一次要变黑的位置
@LOOP
0;JMP

(CLEAN)      //清屏逻辑执行
@SCREEN
D=A
D=M[R0]-D    //如果要变黑的位置为屏幕的开始处,不再执行清屏.
@LOOP
D;JLE
AD=M[R0]-1
M=0
M[R0]=D
@LOOP
0;JMP

