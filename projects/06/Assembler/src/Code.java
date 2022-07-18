import java.util.HashMap;

/**
 * 将Hack汇编语言助记符翻译成二进制码。
 */
public class Code {
    private static final HashMap<String, String> COMP = new HashMap<String, String>(28) {
        {
            put("0", "0101010");
            put("1", "0111111");
            put("-1", "0111010");
            put("D", "0001100");
            put("A", "0110000");
            put("!D", "0001101");
            put("!A", "0110001");
            put("-D", "0001111");
            put("-A", "0110011");
            put("D+1", "0011111");
            put("A+1", "0110111");
            put("D-1", "0001110");
            put("A-1", "0110010");
            put("D+A", "0000010");
            put("D-A", "0010011");
            put("A-D", "0000111");
            put("D&A", "0000000");
            put("D|A", "0010101");
            put("M", "1110000");
            put("!M", "11100011");
            put("-M", "1110011");
            put("M+1", "1110111");
            put("M-1", "1110010");
            put("D+M", "1000010");
            put("D-M", "1010011");
            put("M-D", "1000111");
            put("D&M", "1000000");
            put("D|M", "1010101");
        }
    };
    private static final HashMap<String, String> DEST = new HashMap<String, String>(8) {
        {
            put("null", "000");
            put("M", "001");
            put("D", "010");
            put("MD", "011");
            put("A", "100");
            put("AM", "101");
            put("AD", "110");
            put("AMD", "111");
        }
    };
    private static final HashMap<String, String> JUMP = new HashMap<String, String>(8) {
        {
            put("null", "000");
            put("JGT", "001");
            put("JEQ", "010");
            put("JGE", "011");
            put("JLT", "100");
            put("JNE", "101");
            put("JLE", "110");
            put("JMP", "111");
        }
    };

    /**
     * @param str 助记符
     * @return 3bits的dest助记符的二进制码.
     */
    public static String dest(String str) {
        return DEST.get(str);
    }

    /**
     * @param str 助记符
     * @return 7bits的comp助记符的二进制码.
     */
    public static String comp(String str) {
        return COMP.get(str);
    }

    /**
     * @param str 助记符
     * @return 3bits的jump助记符的二进制码.
     */
    public static String jump(String str) {
        return JUMP.get(str);
    }
}
