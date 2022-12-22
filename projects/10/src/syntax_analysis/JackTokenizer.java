package syntax_analysis;

import com.sun.xml.internal.ws.util.StringUtils;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * 字元转换器模块
 */
public class JackTokenizer {
    private static class Constant {
        private static final String SPACE = " ";
        private static final String KEYWORD = "KEYWORD";
        private static final String SYMBOL = "SYMBOL";
        private static final String IDENTIFIER = "IDENTIFIER";
        private static final String INT_CONST = "INT_CONST";
        private static final String STRING_CONST = "STRING_CONST";
        private static final String CLASS = "CLASS";
        private static final String METHOD = "METHOD";
        private static final String INT = "INT";
        private static final String FUNCTION = "FUNCTION";
        private static final String BOOLEAN = "BOOLEAN";
        private static final String CONSTRUCTION = "CONSTRUCTION";
        private static final String CHAR = "CHAR";
        private static final String VOID = "VOID";
        private static final String VAR = "VAR";
        private static final String STATIC = "STATIC";
        private static final String FIELD = "FIELD";
        private static final String LET = "LET";
        private static final String DO = "DO";
        private static final String IF = "IF";
        private static final String ELSE = "ELSE";
        private static final String WHILE = "WHILE";
        private static final String RETURN = "RETURN";
        private static final String TRUE = "TRUE";
        private static final String FALSE = "FALSE";
        private static final String NULL = "NULL";
        private static final String THIS = "THIS";
    }

    public JackTokenizer(InputStream inputStream) throws IOException {
        StringBuilder sb = new StringBuilder("");
        InputStreamReader inputStreamReader = new InputStreamReader(inputStream);
        //把流中的数据 串成字符串
        int read;
        while ((read = inputStreamReader.read()) != -1) {
            sb.append((char) read);
        }
        //去掉字符串中的注释
        while (sb.indexOf("//") != -1) {
            int start = sb.indexOf("//");
            int end = sb.indexOf("\n", start) + "\n".length();
            sb.delete(start, end);
        }
        while (sb.indexOf("/**") != -1) {
            int start = sb.indexOf("/**");
            int end = sb.indexOf("*/", start) + "*/".length();
            sb.delete(start, end);
        }
        String[] split = sb.toString().split(Constant.SPACE);


        String s = sb.toString();
        System.out.println(s);


    }
}
