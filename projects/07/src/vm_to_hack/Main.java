package vm_to_hack;

import java.io.IOException;
import java.util.List;

public class Main {

    public static void main(String[] args) throws IOException {
        Parser parser = new Parser("");
        List<String> asmCommand = parser.getASMCommands();

    }
}
