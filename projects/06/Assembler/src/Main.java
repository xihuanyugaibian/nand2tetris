import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;

public class Main {
    public static void main(String[] args) throws IOException {
        String filePath = "D:\\fykData\\document\\计算机系统要素\\nand2tetris\\projects\\06\\pong\\PongL.asm";
        Path path = Paths.get(filePath);
        File file = path.toFile();
        File newFile = new File(file.getParentFile(), file.getName().replace(".asm", ".hack"));

        try (BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(newFile)))) {
            for (String s : new Parser(filePath).getBinaryCommands()) {
                bufferedWriter.write(s + "\n");
            }
        }
        newFile.createNewFile();
    }
}
