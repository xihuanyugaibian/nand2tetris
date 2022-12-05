package vm_to_hack;

import java.io.*;
import java.util.List;

public class Main {

    public static void main(String[] args) throws IOException {
        String filePath = "D:\\fykData\\document\\计算机系统要素\\nand2tetris\\projects\\08\\FunctionCalls\\FibonacciElement\\Sys.vm";
        File file = new File(filePath);
        Parser parser = new Parser(file);
        List<String> asmCommand = parser.getASMCommands();

        String filePath1 = "D:\\fykData\\document\\计算机系统要素\\nand2tetris\\projects\\08\\FunctionCalls\\FibonacciElement\\Main.vm";
        File file1 = new File(filePath1);
        Parser parser1 = new Parser(file1);
        List<String> asmCommand1 = parser1.getASMCommands();

        asmCommand.addAll(asmCommand1);


        File newFile = new File(file.getParentFile(), file.getName().replace(".vm", ".asm"));
        try (BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(newFile)))) {
            for (String s : asmCommand) {
                bufferedWriter.write(s);
            }
        }
        newFile.createNewFile();

    }
}
