package vm_to_hack;

import java.util.HashMap;
import java.util.Map;

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

    public CodeWriter(String fileName) {
        this.fileName = fileName;
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
        return "\n//" + command + "\n" + asmCommand;
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
                "A=M-1\n" +//栈顶元素的地址放入A寄存器
                "D=M\n" +//栈顶元素的值放入D寄存器

                "A=A-1\n" +//次栈顶元素的地址放入A寄存器
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
                "M=-1\n" +//VM中-1表示真，替换掉次栈顶元素的值并作为栈顶元素
                "@SP\n" +
                "M=D\n" +//SP指向当前栈顶元素的后一位
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
                "A=M-1\n" +//栈顶元素的地址放入A寄存器
                "D=M\n" +//把栈顶的元素的值放入D寄存器
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
    public String getPushPopAsmCommand(String commandType, String segment, Integer index) {
        String asmCommand = null;
        if (Parser.C_PUSH.equals(commandType)) {
            asmCommand = pushAsmCommand(segment, index);
        }
        if (Parser.C_POP.equals(commandType)) {
            asmCommand = popAsmCommand(segment, index);
        }
        return "\n//" + commandType + ":" + segment + ":" + index + "\n" + asmCommand;
    }

    /**
     * @param segment 虚拟内存段
     * @param index   非负整数，每个segment的index从0开始
     * @return pop操作的指令
     */
    private String popAsmCommand(String segment, Integer index) {
        String asmCommand;
        if (SEGMENT_TEMP.equals(segment) || SEGMENT_POINTER.equals(segment)) {
            asmCommand = "@" + map.get(segment) + "\n" +
                    "D=A\n" +//temp和point中存放的数据作为值
                    "@" + index + "\n" +
                    "D=D+A\n" +
                    "@SP\n" +
                    "A=M\n" +
                    "M=D\n" +//把segment的地址存入当前SP指向的位置。

                    "A=A-1\n" +
                    "D=M\n" +//把栈顶元素的值放入D寄存器
                    "@SP\n" +
                    "A=M\n" +
                    "A=M\n" +
                    "M=D\n" +//把栈顶元素的值 存入 segment的地址中
                    "@SP\n" +
                    "M=M-1\n";
        } else if (SEGMENT_STATIC.equals(segment)) {
            asmCommand = "@SP\n" +
                    "AM=M-1\n" +
                    "D=M\n" +
                    "@" + fileName + "." + index + "\n" +//每当编译器遇到一个新的变量符号的时候，默认该符号表示固定的地址，从16开始。
                    "M=D\n";
        } else {
            asmCommand = "@" + map.get(segment) + "\n" +
                    "D=M\n" +//local,argument，this,that中存放的数据作为地址
                    "@" + index + "\n" +
                    "D=D+A\n" +
                    "@SP\n" +
                    "A=M\n" +
                    "M=D\n" +//把segment的地址存入当前SP指向的位置

                    "A=A-1\n" +
                    "D=M\n" +//把栈顶元素的值放入D寄存器
                    "@SP\n" +
                    "A=M\n" +
                    "A=M\n" +
                    "M=D\n" +//把栈顶元素的值 存入 segment的地址中
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
    private String pushAsmCommand(String segment, Integer index) {
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
        return "";
    }

    /**
     * 编写执行label命令的汇编代码
     *
     * @param label 标签
     * @return
     */
    public String getLabel(String label) {
        String asmCommand = "(" + fileName + "$" + label + ")";
        return asmCommand;
    }

    /**
     * 编写执行goto命令的汇编代码
     *
     * @param label 标签
     * @return
     */
    public String getGoto(String label) {
        String asmCommand = "@" + fileName + "$" + label + "\n" +
                "0;JMP\n";
        return asmCommand;
    }

    /**
     * 编写执行if命令的汇编代码
     *
     * @param label 标签
     * @return
     */
    public String getIf(String label) {
        String asmCommand = "@SP\n" +
                "AM=M-1\n" +
                "D=M\n" +
                "@" + fileName + "$" + label + "\n"
                + "D;JNE\n";
        return asmCommand;
    }

    /**
     * 编写执行call命令的汇编代码
     *
     * @param functionName 方法名
     * @param numArgs      已有参数个数
     * @return
     */
    public String getCall(String functionName, Integer numArgs) {
        String asmCommand = "";
        return asmCommand;
    }

    /**
     * 编写执行return命令的汇编代码
     *
     * @return
     */
    public String getReturn() {
        String asmCommand = "n";
        return asmCommand;
    }

    /**
     * 编写执行function命令的汇编代码
     *
     * @param functionName 方法名
     * @param numLocals    方法参数个数
     * @return
     */
    public String getFunction(String functionName, Integer numLocals) {
        String asmCommand = "(" + functionName + ")";
        return asmCommand;
    }

}
