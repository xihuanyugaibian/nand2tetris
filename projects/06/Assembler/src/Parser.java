import java.io.*;
import java.util.ArrayList;

/**
 * 封装了对输入代码的访问操作。
 * 1.读取汇编语言命令并对其进行解析
 * 2.提供 “方便访问汇编命令成分（域和符号）”的方案
 * 3.去掉所有空格和注释
 */
public class Parser {
    private static final String A_COMMAND = "A_COMMAND";
    private static final String C_COMMAND = "C_COMMAND";
    private static final String L_COMMAND = "L_COMMAND";

    private ArrayList<String> list = new ArrayList<>();
    private int currentIndex;
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
        while ((line = bufferedReader.readLine()) != null) {
            String trim = line.trim();
            if (!trim.isEmpty()) {
                list.add(trim);
            }
        }
        currentIndex = 0;
        currentCommand = null;
    }

    /**
     * 输入当中还有命令。
     *
     * @return true 有，false 无
     */
    public boolean hasMoreCommands() {
        return list.size() - currentIndex > 0;
    }

    /**
     * 从输入当中读取下一条命令，将其当作“当前命令”。
     * 仅当{@link #hasMoreCommands()}为真时，才能调用本程序。最初始的时候，没有“当前命令”。
     */
    public void advance() {
        currentCommand = list.get(currentIndex);
        currentIndex++;
    }


    /**
     * 返回当前命令类型.
     *
     * @return {@link #A_COMMAND},{@link #C_COMMAND},{@link #L_COMMAND}
     */
    public String commandType() {
        char c = currentCommand.charAt(0);
        if ('@' == c) {
            return A_COMMAND;
        }
        if ('(' == c) {
            return L_COMMAND;
        }
        return C_COMMAND;
    }

    /**
     * @return 形如@Xxx或(Xxx)的当前命令的符号或十进制，仅当{@link #commandType()}是A_COMMAND或L_COMMAND时才能调用。
     */
    public String symbol() {
        currentCommand.replace()
        return null;
    }

    /**
     * @return 当前C指令的dest助记符（8种可能的形式），仅当{@link #commandType()}是C_COMMAND时才能调用。
     */
    public String dest() {
        return null;
    }

    /**
     * @return 当前C指令的comp助记符（28种可能的形式），仅当{@link #commandType()}是C_COMMAND时才能调用。
     */
    public String comp() {
        return null;
    }

    /**
     * @return 当前C指令的jump助记符（8种可能的形式），仅当{@link #commandType()}是C_COMMAND时才能调用。
     */
    public String jump() {
        return null;
    }


}
