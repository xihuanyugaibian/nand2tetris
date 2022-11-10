package vm_to_hack;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * 封装了对输入代码的访问操作。
 * 1.读取汇编语言命令并对其进行解析
 * 2.提供 “方便访问汇编命令成分（域和符号）”的方案
 * 3.去掉所有空格和注释
 */
public class Parser {
    public static final String SPACE = " ";
    public static final String C_ARITHMETIC = "C_ARITHMETIC";
    public static final String C_PUSH = "C_PUSH";
    public static final String C_POP = "C_POP";
    public static final String C_LABEL = "C_LABEL";
    public static final String C_GOTO = "C_GOTO";
    public static final String C_IF = "C_IF";
    public static final String C_FUNCTION = "C_FUNCTION";
    public static final String C_RETURN = "C_RETURN";
    public static final String C_CALL = "C_CALL";
    public static final String ADD = "add";
    public static final String SUB = "sub";
    public static final String NEG = "neg";
    public static final String EQ = "eq";
    public static final String GT = "gt";
    public static final String LT = "lt";
    public static final String AND = "and";
    public static final String OR = "or";
    public static final String NOT = "not";
    public static final String PUSH = "push";
    public static final String POP = "pop";
    public static final String LABEL = "label";
    public static final String GOTO = "goto";
    public static final String IF = "if";
    public static final String FUNCTION = "function";
    public static final String CALL = "call";
    public static final String RETURN = "return";
    public static final Map<String, String> commandMapType = new HashMap<>() {
        {
            commandMapType.put(ADD, C_ARITHMETIC);
            commandMapType.put(SUB, C_ARITHMETIC);
            commandMapType.put(NEG, C_ARITHMETIC);
            commandMapType.put(EQ, C_ARITHMETIC);
            commandMapType.put(GT, C_ARITHMETIC);
            commandMapType.put(LT, C_ARITHMETIC);
            commandMapType.put(AND, C_ARITHMETIC);
            commandMapType.put(OR, C_ARITHMETIC);
            commandMapType.put(NOT, C_ARITHMETIC);
            commandMapType.put(PUSH, C_PUSH);
            commandMapType.put(POP, C_POP);
            commandMapType.put(LABEL, C_LABEL);
            commandMapType.put(GOTO, C_GOTO);
            commandMapType.put(IF, C_IF);
            commandMapType.put(FUNCTION, C_FUNCTION);
            commandMapType.put(RETURN, C_RETURN);
            commandMapType.put(CALL, C_CALL);
        }
    };

    private ArrayList<String> commands = new ArrayList<>();
    /**
     * 当前汇编命令的索引位置
     */
    private int currentIndex;
    /**
     * 当前汇编命令
     */
    private String currentCommand;

    public Parser(String filePath) throws IOException {
        this(new FileInputStream(filePath));
    }

    public Parser(InputStream inputStream) throws IOException {
        this(new InputStreamReader(inputStream));
    }

    public Parser(Reader reader) throws IOException {
        BufferedReader bufferedReader = new BufferedReader(reader);
        String line;
        //循环读取VM文件中的信息，一次读一行
        while ((line = bufferedReader.readLine()) != null) {
            //去掉前后的空格，如果是空或者以//开头，表示该行不是正式的指令，跳过该行。
            String command = line.trim();
            if (command.isEmpty() || command.startsWith("//")) {
                continue;
            }
            //VM命令后面可能跟的有空格和注释，
            if (command.contains("//")) {
                int i = command.indexOf("//");
                command = command.substring(0, i).trim();
            }
            commands.add(command);
        }
        //初始化成员变量
        reset();
    }

    /**
     * 输入当中还有命令。
     *
     * @return true 有，false 无
     */
    private boolean hasMoreCommands() {
        return commands.size() - currentIndex > 1;
    }

    /**
     * 从输入当中读取下一条命令，将其当作“当前命令”。
     * 仅当{@link #hasMoreCommands()}为真时，才能调用本程序。最初始的时候，没有“当前命令”。
     */
    private void advance() {
        currentIndex++;
        currentCommand = commands.get(currentIndex);
    }

    private void reset() {
        currentCommand = null;
        currentIndex = -1;
    }

    /**
     * 返回当前命令类型.
     *
     * @return {@link #C_ARITHMETIC},{@link #C_PUSH},{@link #C_POP},{@link #C_LABEL},{@link #C_GOTO},
     * {@link #C_IF},{@link #C_FUNCTION},{@link #C_RETURN},{@link #C_CALL}
     */
    public String commandType() {
        String[] split = currentCommand.split(SPACE);
        return commandMapType.get(split[0]);
    }

    /**
     * 返回当前命令的第一个参数.<br>
     * 如果当前命令类型为{@link #C_ARITHMETIC},则返回命令本身(如add,sub等).<br>
     * 当前命令类型为{@link #C_RETURN}时,不应该调用本程序.
     */
    public String arg1() {
        String[] split = currentCommand.split(SPACE);
        return split[1];
    }

    /**
     * 返回当前命令的第二个参数.<br>
     * 如果当前命令类型为{@link #C_ARITHMETIC},则返回命令本身(如add,sub等).<br>
     * 当前命令类型为{@link #C_RETURN}时,不应该调用本程序.
     */
    public int arg2() {
        String[] split = currentCommand.split(SPACE);
        return Integer.parseInt(split[2]);
    }

}
