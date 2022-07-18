import java.util.HashMap;

public class SymbolTable {
    private static final HashMap<String, Integer> map = new HashMap<>();

    /**
     * 将(symbol,address)配对加入符号表。
     *
     * @param symbol  符号
     * @param address 地址
     */
    public static void addEntry(String symbol, int address) {
        map.put(symbol, address);
    }

    /**
     * 符号表是否包含了指定的symbol?
     *
     * @param symbol 符号
     * @return true 是，false 否
     */
    public static boolean contains(String symbol) {
        return map.containsKey(symbol);
    }

    /**
     * 返回与symbol关联的地址。
     *
     * @param symbol 符号
     * @return 地址
     */
    public static int getAddress(String symbol) {
        return map.get(symbol);
    }

}
