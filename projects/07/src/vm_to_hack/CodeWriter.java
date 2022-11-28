package vm_to_hack;

import java.util.HashMap;
import java.util.Map;

public class CodeWriter {
    private static String SP = "SP";
    private static String LCL = "LCL";
    private static String ARG = "ARG";
    private static String THIS = "THIS";
    private static String THAT = "THAT";
    private static String TEMP = "TEMP";
    private static String SEGMENT_ARGUMENT = "argument";
    private static String SEGMENT_LOCAL = "local";
    private static String SEGMENT_STATIC = "static";
    private static String SEGMENT_CONSTANT = "constant";
    private static String SEGMENT_THIS = "this";
    private static String SEGMENT_THAT = "that";
    private static String SEGMENT_POINTER = "pointer";
    private static String SEGMENT_TEMP = "temp";
    public static final Map<String, String> map = new HashMap<String, String>() {
        {
            this.put(SEGMENT_LOCAL, LCL);
            this.put(SEGMENT_ARGUMENT, ARG);
            this.put(SEGMENT_THIS, THIS);
            this.put(SEGMENT_THAT, THAT);
            this.put(SEGMENT_TEMP, TEMP);
        }
    };
    private int i;

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

    public String writePushPop(String commandType, String segment, Integer index) {
        String asmCommand = null;
        String s = "\n//" + commandType + ":" + segment + ":" + index + "\n";
        if (Parser.C_PUSH.equals(commandType)) {
            if (SEGMENT_CONSTANT.equals(segment)) {
                asmCommand = "@" + index + "\n" +
                        "D=A\n" +
                        "@SP\n" +
                        "A=M\n" +
                        "M=D\n" +
                        "@SP\n" +
                        "M=M+1\n";
            } else {
                asmCommand = "@" + map.get(segment) + "\n" +
                        "D=M\n" +
                        "@index\n" +
                        "A=D+A\n" +
                        "D=M\n" +
                        "@SP\n" +
                        "A=M\n" +
                        "M=D\n" +
                        "@SP\n" +
                        "M=M+1\n";
            }
        }
        if (Parser.C_POP.equals(commandType)) {
            asmCommand = "@" + map.get(segment) + "\n" +
                    "D=M\n" +
                    "@" + index + "\n" +
                    "D=D+A\n" +
                    "@TEMP\n" +
                    "M=D\n" +
                    "@SP\n" +
                    "AM=M-1\n" +
                    "D=M\n" +
                    "@TEMP\n" +
                    "A=M\n" +
                    "M=D\n";
        }
        return s + asmCommand;
    }

}
