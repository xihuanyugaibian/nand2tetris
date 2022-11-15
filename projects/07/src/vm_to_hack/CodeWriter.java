package vm_to_hack;

public class CodeWriter {
    private int SP = 0;
    private int LCL = 1;
    private int ARG = 2;
    private int THIS = 3;
    private int THAT = 4;
    private static String SEGMENT_ARGUMENT = "argument";
    private static String SEGMENT_LOCAL = "local";
    private static String SEGMENT_STATIC = "static";
    private static String SEGMENT_CONSTANT = "constant";
    private static String SEGMENT_THIS = "this";
    private static String SEGMENT_THAT = "that";
    private static String SEGMENT_POINTER = "pointer";
    private static String SEGMENT_TEMP = "temp";


    public String writeArithmetic(String command) {
        String asmCommand = null;
        if (Parser.ADD.equals(command)) {
            asmCommand = "@SP\n" +
                    "A=M-1\n" +
                    "D=M\n" +
                    "A=A-1\n" +
                    "D=D+M\n" +
                    "M=D\n" +
                    "D=A\n" +
                    "@SP\n" +
                    "M=D\n";
        }
        return asmCommand;
    }

    public String writePushPop(String commandType, String segment, Integer index) {
        String asmCommand = null;
        if (Parser.C_PUSH.equals(commandType)) {
            if (SEGMENT_CONSTANT.equals(segment)) {
                asmCommand = "@" + index + "\n" +
                        "D=A\n" +
                        "@SP\n" +
                        "A=M\n" +
                        "M=D\n" +
                        "@SP\n" +
                        "M=M+1\n";
            }

        }
        if (Parser.C_POP.equals(commandType)) {
            asmCommand = "@1\n" +
                    "D=A\n" +
                    "@SP\n" +
                    "M=M-D\n" +
                    "M=D\n" +
                    "@1\n" +
                    "D=A\n" +
                    "@SP\n" +
                    "M=D+M\n";
        }
        return asmCommand;
    }

}
