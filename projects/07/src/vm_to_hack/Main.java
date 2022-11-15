package vm_to_hack;

import java.io.*;
import java.util.List;

public class Main {

    public static void main(String[] args) throws IOException {
        String filePath = "D:\\workspace\\IdeaProjects\\nand2tetris\\projects\\07\\StackArithmetic\\SimpleAdd\\SimpleAdd.vm";
        String parentPath = "D:\\workspace\\IdeaProjects\\nand2tetris\\projects\\07\\StackArithmetic\\SimpleAdd\\SimpleAdd.vm";
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
