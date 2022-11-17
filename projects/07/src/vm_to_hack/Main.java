package vm_to_hack;

import java.io.*;
import java.util.List;

public class Main {

    public static void main(String[] args) throws IOException {
        String filePath = "D:\\fykData\\document\\计算机系统要素\\nand2tetris\\projects\\07\\StackArithmetic\\StackTest\\StackTest.vm";
        Parser parser = new Parser(filePath);
        List<String> asmCommand = parser.getASMCommands();

        File file = new File(filePath);
        File newFile = new File(file.getParentFile(), file.getName().replace(".vm", ".asm"));
        try (BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(newFile)))) {
            for (String s : asmCommand) {
                bufferedWriter.write(s);
            }
        }
        newFile.createNewFile();

    }
}
