package assembler;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * 封装了对输入代码的访问操作。
 * 1.读取汇编语言命令并对其进行解析
 * 2.提供 “方便访问汇编命令成分（域和符号）”的方案
 * 3.去掉所有空格和注释
 */
public class Parser {
    public static final String A_COMMAND = "A_COMMAND";
    public static final String C_COMMAND = "C_COMMAND";
    public static final String L_COMMAND = "L_COMMAND";
    private static final HashMap<Integer, String> fillZeroMap = new HashMap(15);

    static {
        fillZeroMap.put(1, "0");
        fillZeroMap.put(2, "00");
        fillZeroMap.put(3, "000");
        fillZeroMap.put(4, "0000");
        fillZeroMap.put(5, "00000");
        fillZeroMap.put(6, "000000");
        fillZeroMap.put(7, "0000000");
        fillZeroMap.put(8, "00000000");
        fillZeroMap.put(9, "000000000");
        fillZeroMap.put(10, "0000000000");
        fillZeroMap.put(11, "00000000000");
        fillZeroMap.put(12, "000000000000");
        fillZeroMap.put(13, "0000000000000");
        fillZeroMap.put(14, "00000000000000");
        fillZeroMap.put(15, "000000000000000");
    }

    private ArrayList<String> list = new ArrayList<>();
    /**
     * 当前汇编命令的索引位置
     */
    private int currentIndex;
    /**
     * 当前汇编命令
     */
    private String currentCommand;
    /**
     * 当前汇编命令在内存中的地址，程序命令在内存中的地址从0开始。
     * (Xxx)标签声明 从汇编语言来说算是一个命令，从占用内存来说 不占用内存地址。
     */
    private int currentCommandAddress;


    public Parser(String filePath) throws IOException {
        this(new FileInputStream(filePath));
    }

    public Parser(InputStream inputStream) throws IOException {
        this(new InputStreamReader(inputStream));
    }

    public Parser(Reader reader) throws IOException {
        BufferedReader bufferedReader = new BufferedReader(reader);
        String line;
        //循环读取文件汇编文件中的信息，一次读一行
        while ((line = bufferedReader.readLine()) != null) {
            //去掉前后的空格，如果是空或者以//开头，表示该行不是正式的指令，跳过该行。
            String trim = line.trim();
            if (trim.isEmpty() || trim.startsWith("//")) {
                continue;
            }
            //正式指令后面可能跟的有注释，去掉注释还有多余的空格
            if (trim.contains("//")) {
                trim.replace(" ", "");
                int i = trim.indexOf("//");
                trim = trim.substring(0, i);
            }
            list.add(trim);
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
        return list.size() - currentIndex > 1;
    }

    /**
     * 从输入当中读取下一条命令，将其当作“当前命令”。
     * 仅当{@link #hasMoreCommands()}为真时，才能调用本程序。最初始的时候，没有“当前命令”。
     */
    private void advance() {
        currentIndex++;
        currentCommandAddress++;
        currentCommand = list.get(currentIndex);
        //如果上一条汇编时标签声明，该行不占内存，实际地址应该-1.起始的没有上一条。
        if (currentIndex > 0) {
            String lastCommand = list.get(currentIndex - 1);
            String commandType = commandType(lastCommand);
            if (L_COMMAND.equals(commandType)) {
                currentCommandAddress--;
            }
        }
    }

    private void reset() {
        currentCommand = null;
        currentIndex = -1;
        currentCommandAddress = -1;
    }


    /**
     * 返回当前命令类型.
     *
     * @param command 指令
     * @return {@link #A_COMMAND},{@link #C_COMMAND},{@link #L_COMMAND}
     */
    private String commandType(String command) {
        if (command.startsWith("@")) {
            return A_COMMAND;
        }
        if (command.startsWith("(")) {
            return L_COMMAND;
        }
        return C_COMMAND;
    }

    /**
     * 返回当前命令类型.
     *
     * @return {@link #A_COMMAND},{@link #C_COMMAND},{@link #L_COMMAND}
     */
    private String commandType() {
        return commandType(currentCommand);
    }

    /**
     * @return 形如@Xxx或(Xxx)的当前命令的符号或十进制，仅当{@link #commandType()}是A_COMMAND或L_COMMAND时才能调用。
     */
    private String symbol() {
        String commandType = commandType();
        if (A_COMMAND.equals(commandType)) {
            return currentCommand.substring(1);
        } else {
            //L指令(标签声明)只要括号中间的。表示当前命令所在的地址。
            return currentCommand.substring(1, currentCommand.length() - 1);
        }
    }

    /**
     * @return 当前C指令的dest助记符（8种可能的形式），仅当{@link #commandType()}是C_COMMAND时才能调用。
     */
    private String dest() {
        int indexOf = currentCommand.indexOf("=");
        if (indexOf > 0) {
            return currentCommand.substring(0, indexOf);
        }
        return "null";
    }

    /**
     * @return 当前C指令的comp助记符（28种可能的形式），仅当{@link #commandType()}是C_COMMAND时才能调用。
     */
    private String comp() {
        int i0 = currentCommand.indexOf("=");
        int i1 = currentCommand.indexOf(";");
        i0 = i0 != -1 ? i0 + 1 : 0;
        i1 = i1 != -1 ? i1 : currentCommand.length();
        return currentCommand.substring(i0, i1);
    }

    /**
     * @return 当前C指令的jump助记符（8种可能的形式），仅当{@link #commandType()}是C_COMMAND时才能调用。
     */
    private String jump() {
        int indexOf = currentCommand.indexOf(";");
        if (indexOf > 0) {
            return currentCommand.substring(indexOf + 1);
        }
        return "null";
    }

    /**
     * 初始化符号,将汇编语言中自定义的符号初始化到符号表{@link SymbolTable}中
     */
    private void initSymbol() {
        //初始化标签：把伪命令的符号存入符号表
        while (this.hasMoreCommands()) {
            this.advance();
            if (L_COMMAND.equals(this.commandType())) {
                String symbol = this.symbol();
                SymbolTable.addEntry(symbol, this.currentCommandAddress);
            }
        }
        this.reset();
        //初始化变量
        int variableAddress = 16;
        while (this.hasMoreCommands()) {
            this.advance();
            if (A_COMMAND.equals(this.commandType())) {
                String symbol = this.symbol();
                boolean digit = Character.isDigit(symbol.charAt(0));
                if (!digit && !SymbolTable.contains(symbol)) {
                    SymbolTable.addEntry(symbol, variableAddress++);
                }
            }
        }
        reset();
    }

    /**
     * 将不满16位的二进制指令，左边补零为16位的二进制指令。
     *
     * @param binaryCommand 二进制的指令
     * @return 16位的二进制指令。
     */
    private String get16bitsBinaryCommand(String binaryCommand) {
        if (binaryCommand.length() < 16) {
            return fillZeroMap.get(16 - binaryCommand.length()) + binaryCommand;
        }
        return binaryCommand;
    }

    /**
     * 获取解析后的指令集合
     *
     * @return 指令集合
     */
    public List<String> getBinaryCommands() {
        initSymbol();

        List<String> list = new ArrayList<>();
        while (this.hasMoreCommands()) {
            this.advance();
            String commandType = this.commandType();
            if (A_COMMAND.equals(commandType)) {
                String symbol = this.symbol();
                String binaryString = null;
                if (SymbolTable.contains(symbol)) {
                    binaryString = Integer.toBinaryString(SymbolTable.getAddress(symbol));
                } else {
                    binaryString = Integer.toBinaryString(Integer.parseInt(symbol));
                }
                list.add(get16bitsBinaryCommand(binaryString));
                continue;
            }
            if (C_COMMAND.equals(commandType)) {
                String dest = Code.dest(this.dest());
                String comp = Code.comp(this.comp());
                String jump = Code.jump(this.jump());
                list.add("111" + comp + dest + jump);
            }
        }
        return list;
    }
}
