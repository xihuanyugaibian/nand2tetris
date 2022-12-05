package vm_to_hack;

import java.util.HashMap;
import java.util.Map;

import static vm_to_hack.Parser.*;

public class CodeWriter {
    private static final String SP = "SP";
    private static final String LCL = "LCL";
    private static final String ARG = "ARG";
    private static final String THIS = "THIS";
    private static final String THAT = "THAT";
    private static final String TEMP = "5";
    private static final String SEGMENT_ARGUMENT = "argument";
    private static final String SEGMENT_LOCAL = "local";
    private static final String SEGMENT_STATIC = "static";
    private static final String SEGMENT_CONSTANT = "constant";
    private static final String SEGMENT_THIS = "this";
    private static final String SEGMENT_THAT = "that";
    private static final String SEGMENT_POINTER = "pointer";
    private static final String SEGMENT_TEMP = "temp";
    public static final Map<String, String> map = new HashMap<String, String>() {
        {
            this.put(SEGMENT_LOCAL, LCL);
            this.put(SEGMENT_ARGUMENT, ARG);
            this.put(SEGMENT_THIS, THIS);
            this.put(SEGMENT_THAT, THAT);
            this.put(SEGMENT_TEMP, TEMP);
            this.put(SEGMENT_POINTER, THIS);
        }
    };
    private int i;
    private String fileName;
    private String nearestFunctionName;

    public CodeWriter(String fileName) {
        this.fileName = fileName;
    }

    public String getAsmCommand(String vmCommand) {
        String command;
        String arg1 = null;
        String arg2 = "0";
        String[] split = vmCommand.split(SPACE);
        command = split[0];
        if (split.length >= 2) {
            arg1 = split[1];
        }
        if (split.length >= 3) {
            arg2 = split[2];
        }

        String asmCommand = null;
        String commandType = Parser.commandType(command);
        if (C_ARITHMETIC.equals(commandType)) {
            asmCommand = this.writeArithmetic(command);
        }
        if (C_POP.equals(commandType) || C_PUSH.equals(commandType)) {
            asmCommand = this.getPushPopAsmCommand(commandType, arg1, arg2);
        }
        if (C_LABEL.equals(commandType)) {
            asmCommand = this.getLabel(arg1);
        }
        if (C_IF.equals(commandType)) {
            asmCommand = this.getIf(arg1);
        }
        if (C_GOTO.equals(commandType)) {
            asmCommand = this.getGoto(arg1);
        }
        if (C_RETURN.equals(commandType)) {
            asmCommand = this.getReturn();
        }
        if (C_FUNCTION.equals(commandType)) {
            asmCommand = this.getFunction(arg1, arg2);
        }
        if (C_CALL.equals(commandType)) {
            asmCommand = this.getCall(arg1, arg2);
        }
        return asmCommand;
    }

    public String writeArithmetic(String command) {
        String asmCommand = null;
        if (Parser.ADD.equals(command)) {
            asmCommand = add();
        }
        if (Parser.SUB.equals(command)) {
            asmCommand = sub();
        }
        if (Parser.NEG.equals(command)) {
            asmCommand = neg();
        }
        if (Parser.EQ.equals(command)) {
            asmCommand = eq(++i);
        }
        if (Parser.GT.equals(command)) {
            asmCommand = gt(++i);
        }
        if (Parser.LT.equals(command)) {
            asmCommand = lt(++i);
        }
        if (Parser.AND.equals(command)) {
            asmCommand = and();
        }
        if (Parser.OR.equals(command)) {
            asmCommand = or();
        }
        if (Parser.NOT.equals(command)) {
            asmCommand = not();
        }
        return asmCommand.replaceFirst("\n", "      //" + command + "\n");
    }

    private String sub() {
        return "@SP\n" +
                "A=M-1\n" +
                "D=M\n" +
                "A=A-1\n" +
                "D=M-D\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M-1\n";
    }

    private String neg() {
        return "@SP\n" +
                "A=M-1\n" +
                "D=M\n" +
                "M=-D\n";
    }

    private String eq(int i) {
        return "@SP\n" +
                "A=M-1\n" +     //栈顶元素的地址放入A寄存器
                "D=M\n" +     //栈顶元素的值放入D寄存器

                "A=A-1\n" +     //次栈顶元素的地址放入A寄存器
                "D=M-D\n" +
                "@EQ" + i + "\n" +
                "D;JEQ\n" +
                "@SP\n" +
                "AD=M-1\n" +
                "A=A-1\n" +
                "M=0\n" +
                "@SP\n" +
                "M=D\n" +
                "@END" + i + "\n" +
                "0;JMP\n" +

                "(EQ" + i + ")\n" +
                "@SP\n" +
                "AD=M-1\n" +
                "A=A-1\n" +
                "M=-1\n" +     //VM中-1表示真，替换掉次栈顶元素的值并作为栈顶元素
                "@SP\n" +
                "M=D\n" +     //SP指向当前栈顶元素的后一位
                "(END" + i + ")\n";
    }

    private String gt(int i) {
        return "@SP\n" +
                "A=M-1\n" +
                "D=M\n" +
                "A=A-1\n" +
                "D=M-D\n" +
                "@GT" + i + "\n" +
                "D;JGT\n" +
                "@SP\n" +
                "AD=M-1\n" +
                "A=A-1\n" +
                "M=0\n" +
                "@SP\n" +
                "M=D\n" +
                "@END" + i + "\n" +
                "0;JMP\n" +

                "(GT" + i + ")\n" +
                "@SP\n" +
                "AD=M-1\n" +
                "A=A-1\n" +
                "M=-1\n" +
                "@SP\n" +
                "M=D\n" +
                "(END" + i + ")\n";
    }

    private String lt(int i) {
        return "@SP\n" +
                "A=M-1\n" +
                "D=M\n" +
                "A=A-1\n" +
                "D=M-D\n" +
                "@LT" + i + "\n" +
                "D;JLT\n" +
                "@SP\n" +
                "AD=M-1\n" +
                "A=A-1\n" +
                "M=0\n" +
                "@SP\n" +
                "M=D\n" +
                "@END" + i + "\n" +
                "0;JMP\n" +

                "(LT" + i + ")\n" +
                "@SP\n" +
                "AD=M-1\n" +
                "A=A-1\n" +
                "M=-1\n" +
                "@SP\n" +
                "M=D\n" +
                "(END" + i + ")\n";
    }

    private String and() {
        return "@SP\n" +
                "A=M-1\n" +
                "D=M\n" +
                "A=A-1\n" +
                "D=D&M\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M-1\n";
    }

    private String or() {
        return "@SP\n" +
                "A=M-1\n" +
                "D=M\n" +
                "A=A-1\n" +
                "D=D|M\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M-1\n";
    }

    private String not() {
        return "@SP\n" +
                "A=M-1\n" +
                "D=M\n" +
                "D=!D\n" +
                "M=D\n";
    }

    private String add() {
        return "@SP\n" +
                "A=M-1\n" +     //栈顶元素的地址放入A寄存器
                "D=M\n" +     //把栈顶的元素的值放入D寄存器
                "A=A-1\n" +
                "D=D+M\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M-1\n";
    }

    /**
     * 将给定的vm指令翻译为asm指令。
     *
     * @param commandType 指令类型:{@link Parser#C_PUSH}, {@link Parser#C_POP}
     * @param segment     虚拟内存段
     * @param index       非负整数，每个segment的index从0开始
     * @return
     */
    public String getPushPopAsmCommand(String commandType, String segment, String index) {
        String asmCommand = null;
        if (Parser.C_PUSH.equals(commandType)) {
            asmCommand = pushAsmCommand(segment, index);
        }
        if (Parser.C_POP.equals(commandType)) {
            asmCommand = popAsmCommand(segment, index);
        }
        return asmCommand.replaceFirst("\n", "     //" + commandType + ":" + segment + ":" + index + "\n");
    }

    /**
     * @param segment 虚拟内存段
     * @param index   非负整数，每个segment的index从0开始
     * @return pop操作的指令
     */
    private String popAsmCommand(String segment, String index) {
        String asmCommand;
        if (SEGMENT_TEMP.equals(segment) || SEGMENT_POINTER.equals(segment)) {
            asmCommand = "@" + map.get(segment) + "\n" +
                    "D=A\n" +     //temp和point中存放的数据作为值
                    "@" + index + "\n" +
                    "D=D+A\n" +
                    "@SP\n" +
                    "A=M\n" +
                    "M=D\n" +     //把segment的地址存入当前SP指向的位置。

                    "A=A-1\n" +
                    "D=M\n" +     //把栈顶元素的值放入D寄存器
                    "@SP\n" +
                    "A=M\n" +
                    "A=M\n" +
                    "M=D\n" +     //把栈顶元素的值 存入 segment的地址中
                    "@SP\n" +
                    "M=M-1\n";
        } else if (SEGMENT_STATIC.equals(segment)) {
            asmCommand = "@SP\n" +
                    "AM=M-1\n" +
                    "D=M\n" +
                    "@" + fileName + "." + index + "\n" +     //每当编译器遇到一个新的变量符号的时候，默认该符号表示固定的地址，从16开始。
                    "M=D\n";
        } else {
            asmCommand = "@" + map.get(segment) + "\n" +
                    "D=M\n" +     //local,argument，this,that中存放的数据作为地址
                    "@" + index + "\n" +
                    "D=D+A\n" +
                    "@SP\n" +
                    "A=M\n" +
                    "M=D\n" +     //把segment的地址存入当前SP指向的位置

                    "A=A-1\n" +
                    "D=M\n" +     //把栈顶元素的值放入D寄存器
                    "@SP\n" +
                    "A=M\n" +
                    "A=M\n" +
                    "M=D\n" +     //把栈顶元素的值 存入 segment的地址中
                    "@SP\n" +
                    "M=M-1\n";
        }
        return asmCommand;
    }

    /**
     * @param segment 虚拟内存段
     * @param index   非负整数，每个segment的index从0开始
     * @return push操作的指令
     */
    private String pushAsmCommand(String segment, String index) {
        String asmCommand;
        if (SEGMENT_CONSTANT.equals(segment)) {
            asmCommand = "@" + index + "\n" +
                    "D=A\n" +
                    "@SP\n" +
                    "A=M\n" +
                    "M=D\n" +
                    "@SP\n" +
                    "M=M+1\n";
        } else if (SEGMENT_TEMP.equals(segment) || SEGMENT_POINTER.equals(segment)) {
            asmCommand = "@" + map.get(segment) + "\n" +
                    "D=A\n" +
                    "@" + index + "\n" +
                    "A=D+A\n" +
                    "D=M\n" +
                    "@SP\n" +
                    "A=M\n" +
                    "M=D\n" +
                    "@SP\n" +
                    "M=M+1\n";
        } else if (SEGMENT_STATIC.equals(segment)) {
            asmCommand = "@" + fileName + "." + index + "\n" +
                    "D=M\n" +
                    "@SP\n" +
                    "A=M\n" +
                    "M=D\n" +
                    "@SP\n" +
                    "M=M+1\n";
            if (SP.equals(index) || LCL.equals(index) || ARG.equals(index) || THIS.equals(index) || THAT.equals(index)) {
                //SP LCL  ARG  THIS  THAT 分别代表RAM[0] RAM[1] RAM[2] RAM[3] RAM[4]
                //这里把他们也作为Static类型，不过区别是这几个所有的文件都公用。所以把fileName去掉。
                asmCommand = asmCommand.replaceFirst(fileName + ".", "");
            }
        } else {
            asmCommand = "@" + map.get(segment) + "\n" +
                    "D=M\n" +
                    "@" + index + "\n" +
                    "A=D+A\n" +
                    "D=M\n" +
                    "@SP\n" +
                    "A=M\n" +
                    "M=D\n" +
                    "@SP\n" +
                    "M=M+1\n";
        }
        return asmCommand;
    }

    /**
     * 编写执行VM初始化的汇编代码即引导程序，该代码必须被至于文件开头
     *
     * @return
     */
    private String getInit() {
        return "@261     //Init\n" +
                "D=A\n" +
                "@SP\n" +
                "M=D\n";
    }

    /**
     * 编写执行label命令的汇编代码
     *
     * @param label 标签
     * @return
     */
    public String getLabel(String label) {
        return "(" + nearestFunctionName + "$" + label + ")\n";
    }

    /**
     * 编写执行goto命令的汇编代码
     *
     * @param label 标签
     * @return
     */
    public String getGoto(String label) {
        return "@" + nearestFunctionName + "$" + label + "     //go:" + label + "\n" +
                "0;JMP\n";
    }

    /**
     * 编写执行if命令的汇编代码
     *
     * @param label 标签
     * @return
     */
    public String getIf(String label) {
        return "@SP\n" +
                "AM=M-1\n" +
                "D=M\n" +
                "@" + nearestFunctionName + "$" + label + "     //if_go:" + label + "\n"
                + "D;JNE\n";
    }

    /**
     * 编写执行call命令的汇编代码
     *
     * @param functionName 方法名
     * @param numArgs      已有参数个数
     * @return
     */
    public String getCall(String functionName, String numArgs) {
        ++i;
        return (this.getAsmCommand("push constant returnAddress:" + functionName + i) +
                "@LCL   //push LCL\n" +
                "D=M\n" +
                "@SP\n" +
                "A=M\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M+1\n" +

                "@ARG   //push ARG\n" +
                "D=M\n" +
                "@SP\n" +
                "A=M\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M+1\n" +

                "@THIS   //push THIS\n" +
                "D=M\n" +
                "@SP\n" +
                "A=M\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M+1\n" +

                "@THAT   //push THAT\n" +
                "D=M\n" +
                "@SP\n" +
                "A=M\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M+1\n" +

                "@SP     //LCL=SP\n" +
                "D=M\n" +
                "@LCL\n" +
                "M=D\n" +     //LCL=SP
                "@" + Integer.parseInt(numArgs) + "     //ARG=SP-n-5\n" +
                "D=D-A\n" +
                "@5\n" +
                "D=D-A\n" +
                "@ARG\n" +
                "M=D\n" +     //ARG=SP-n-5
                "@" + functionName + "\n" +
                "0;JMP\n" +
                "(returnAddress:" + functionName + i + ")" + "     //call:" + functionName + ":" + numArgs + "\n");
    }

    /**
     * 编写执行return命令的汇编代码
     *
     * @return
     */
    public String getReturn() {
        /*
        LCL地址保存的是被调用方法 LCL段的起始地址也是 被调用方法的起始地址
        被调用方法开始地址前第1个地址 保存调用者THAT段指针
        被调用方法开始地址前第2个地址 保存调用者THIS段指针
        被调用方法开始地址前第3个地址 保存调用者ARG段指针
        被调用方法开始地址前第4个地址 保存调用者LCL段指针
        被调用方法开始地址前第5个地址 保存被调用者结束后，调用者继续执行的地址

        ARG地址保存的调用者传入参数的首地址

        return命令
        1 临时变量保存LCL地址的值
        2 LCL地址的值-5 得到返回地址 用于跳转
        3 方法的结果 传给被调用者即 替换掉ARG中的数据
        4 恢复调用者SP THAT THIS ARG LCL 的值
        5 调转到返回地址  让调用者继续执行
        */
        return "@LCL   //return FRAME=LCL\n" +
                "D=M\n" +
                "@FRAME\n" +
                "M=D\n" +

                "@5     //RET=*(FRAME-5)\n" +
                "A=D-A\n" +
                "D=M\n" +
                "@RET\n" +
                "M=D\n" +

                "@SP        //*ARG=pop()\n" +
                "A=M-1\n" +
                "D=M\n" +
                "@ARG\n" +
                "A=M\n" +
                "M=D\n" +

                "D=A+1\n" +
                "@SP\n" +
                "M=D\n" +

                "@FRAME\n" +
                "D=M\n" +
                "@1\n" +
                "A=D-A\n" +
                "D=M\n" +
                "@THAT\n" +
                "M=D\n" +

                "@FRAME\n" +
                "D=M\n" +
                "@2\n" +
                "A=D-A\n" +
                "D=M\n" +
                "@THIS\n" +
                "M=D\n" +

                "@FRAME\n" +
                "D=M\n" +
                "@3\n" +
                "A=D-A\n" +
                "D=M\n" +
                "@ARG\n" +
                "M=D\n" +

                "@FRAME\n" +
                "D=M\n" +
                "@4\n" +
                "A=D-A\n" +
                "D=M\n" +
                "@LCL\n" +
                "M=D\n" +

                "@RET   //goto RET\n" +
                "A=M\n" +
                "0;JMP\n";
    }

    /**
     * 编写执行function命令的汇编代码
     *
     * @param functionName 方法名
     * @param numLocals    方法参数个数
     * @return
     */
    public String getFunction(String functionName, String numLocals) {
        nearestFunctionName = functionName;
        if ("Sys.init".equals(functionName)) {
            return getInit();
        }
        StringBuilder asmCommand = new StringBuilder("(" + functionName + ")    //" + functionName + ":" + numLocals + "\n");
        for (int j = 0; j < Integer.parseInt(numLocals); j++) {
            asmCommand.append(this.getAsmCommand("push constant 0"));
        }
        return asmCommand.toString();
    }

}
