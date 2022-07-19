import java.util.HashMap;

public class SymbolTable {
    private static final HashMap<String, Integer> map = new HashMap<>();

    static {
        map.put("SP", 0);
        map.put("LCL", 1);
        map.put("ARG", 2);
        map.put("THIS", 3);
        map.put("THAT", 4);
        map.put("R0", 0);
        map.put("R1", 1);
        map.put("R2", 2);
        map.put("R3", 3);
        map.put("R4", 4);
        map.put("R5", 5);
        map.put("R6", 6);
        map.put("R7", 7);
        map.put("R8", 8);
        map.put("R9", 9);
        map.put("R10", 10);
        map.put("R11", 11);
        map.put("R12", 12);
        map.put("R13", 13);
        map.put("R14", 14);
        map.put("R15", 15);
        map.put("SCREEN", 16384);
        map.put("KBD", 24567);
    }


    /**
     * 将(symbol,address)配对加入符号表。
     *
     * @param symbol  符号
     * @param address 地址
     */
    public static void addEntry(String symbol, int address) {
        map.put(symbol.toUpperCase(), address);
    }

    /**
     * 符号表是否包含了指定的symbol?
     *
     * @param symbol 符号
     * @return true 是，false 否
     */
    public static boolean contains(String symbol) {
        return map.containsKey(symbol.toUpperCase());
    }

    /**
     * 返回与symbol关联的地址。
     *
     * @param symbol 符号
     * @return 地址
     */
    public static int getAddress(String symbol) {
        return map.get(symbol.toUpperCase());
    }

}
