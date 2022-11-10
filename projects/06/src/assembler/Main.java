package assembler;

import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

/**
 * 程序开始地方<br>
 * 在filePath中修改汇编程序文件的绝对路径，会在同一个目录下生成对应的二进制指令的文件。<br>
 * 开发思路：<br>
 * 1.Main 作为主程序，程序开始的地方，负责文件信息的输出和输出<br>
 * 2.Parser 主要类，负责解析汇编文件，把汇编指令转换位二进制指令<br>
 * &nbsp;&nbsp; 2.1 符号区分标签和变量，声明标签的汇编命令表示的是其后命令所在的内存地址。需要跳转的时候，就跳转到这个地址代表的指令处。<br>
 * &nbsp;&nbsp; 2.2 A指令 @后面可能是十进制数值，可能是变量（表示一个用于存放数据的地址），可能是标签（表示一个用于调转的地址，该地址内容是指令不是数据）<br>
 * 3.Code C指令 汇编与二进制之间的对用关系，供Parser解析时调用<br>
 * 4.SymbolTable  预定义符号，标签符号，变量符号三种符号与内存地址的对应关系 供Parser解析时调用
 */
public class Main {
    public static void main(String[] args) throws IOException {
        String filePath = "D:\\fykData\\document\\计算机系统要素\\nand2tetris\\projects\\06\\pong\\Pong.asm";
        Path path = Paths.get(filePath);
        File file = path.toFile();
        File newFile = new File(file.getParentFile(), file.getName().replace(".asm", ".hack"));

        //获取汇编解析后的二进制指令
        List<String> binaryCommands = new Parser(filePath).getBinaryCommands();
        //将二进制指令写入新建文件中
        try (BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(newFile)))) {
            for (String s : binaryCommands) {
                bufferedWriter.write(s + "\n");
            }
        }
        newFile.createNewFile();
    }
}
