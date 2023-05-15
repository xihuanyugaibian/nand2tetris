package syntax_analysis;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * 建立和调用其他模块的顶层驱动模块
 */
public class JackAnalyzer {
    public static void main(String[] args) throws IOException {
        String path = "D:\\workspace\\IdeaProjects\\nand2tetris\\projects\\10\\ArrayTest\\Main.jack";
        FileInputStream fileInputStream = new FileInputStream(path);
        JackTokenizer jackTokenizer = new JackTokenizer(fileInputStream);

    }
}
