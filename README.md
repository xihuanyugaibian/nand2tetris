# 计算机系统要素

## 一、布尔逻辑

1. 布尔代数：用来处理布尔型数值即二进制数值，因为计算机硬件基于二进制数据的表示和处理
    * 真值表示法：描述布尔函数最简单的方法就是枚举出函数所有可能的输入变量组合，然后写出每一种组合 所对应的函数输出值。
    * 布尔表达式：出了真值表示法，布尔函数还可以在输入变量上使用布尔算子来描述。基本的布尔算子有 and or not
    * 规范表示法：每个布尔函数都至少由一个布尔表达式来描述，称之为规范表示法>
      > 从函数的真值表出发，关注函数值为1的行。对于每一行，构建一种表达式来确定所有输出变量的值，即通过对两个字面量通过and操作来得到。 最后把所有函数值为1的表达式用or联合起来，就可以得到与真值表一致的布尔表达式。  
      每个布尔函数不管多复杂，都可以只用三个布尔算子 and or not 来完全表达。
2. 门逻辑:门是用来实现布尔函数的物理设备。
    1. Nand门：Nand(ab)=~(ab)
    1. 基本逻辑门:每个门都可仅由Nand门来组合。
       > Not(a)=~a=~(a1)=Nand(a,1)  
       And(a,b)=ab=~(~(ab))=~(~(ab)1)==Nand(Nand(a,b),1)  
       Or(a,b)=a+b=~~(a+b)=~(~a~b)=Nand(~a,~b)=Nand(Not(a),Not(b))=Nand(Nand(a,1),Nand(b,1))
       Xor(a,b)=a~b+~ab=Or(a~b,~ab)=Or(And(a,~b),And(~a,b))=···  
       Multiplexor(a,b,sel)=a·~sel+b·sel  
       Demultiplexor(in,sel):输出a=a·~sel,输出b=a·sel  
       And Not Or都可以换成基本的Nand，又无论多复杂的布尔函数都可以由And Not Or完全表达，即任何复杂的逻辑电路都能只有Nand门组成。 可以通过规范表示法列出表达式然后再化简，表达式越简单使用的硬件越少。
    1. 多位基本门：输入输出端是多位的二进制。Not16,And16,Or16,Mux16
    1. 多通道逻辑门：任意数量的输入或者输出。
        1. Or8Way:一个输入端8位，一个输出端一位
        1. Mux4Way16:5个输入端4个16位1个2位(选择位)，一个输出端16位
        1. Mux8Way16:9个输入端8个16位1个3位(选择位)，一个输出端16位
        1. DNux4Way:
        1. DMux8Way:

## 二、布尔运算

1. 半加器：用来进行两个位加法
1. 全加器：用来进行三个位加法
1. 加法器：用来进行两个 n位加法
1. 增量器：对指定的数字+1
1. ALU：两个16位输入，一个16位输出，外加6个控制位，可以完成2^6=64种不同的操作。实际只实现了18种。

## 三、时序逻辑

> 计算机不仅要能计算值，好要能存取数据。需要配备记忆单元，记忆单元是由时序芯片组成的。

1. 时钟：大部分计算机中，时间的流逝用主时钟表示，它提供连续的交变信号序列，在两个信号值0-1交替变换。  
   两个相邻的上升沿之间的时间间隙成为时钟周期，每个时钟周期为一个离散的时间单元，通过硬件电路，这个信号同时被传送到计算机平台的每个时序芯片中。
1. 触发器：最基本的时序单元。
   > 数据触发器（DFF D触发器）：将前一个时间周期的输入值作为当前周期的输出。
1. 寄存器：具有记忆功能的设备，能够存储某一时刻的值。多位寄存器就可以保存多个比特。
1. 内存：多个多位寄存器+地址。通过地址读取指定的多位寄存器中的值。
1. 计数器：一种时序芯片，每经过一个时间周期自增1

## 四、机器语言

1. 机器语言：一种约定的形式，利用处理器和寄存器来操作内存。
1. 用来储存数据和指令的硬件设备，内存都有一个连续的固定宽度的单元序列即内存单元，每个内存单元都有一个唯一的地址。  
   对于独立的内存单元（数据项或是指令），可以通过它的地址来描述。
1. 处理器：执行一组固定基本操作的设备，有算数操作、逻辑操作、内存存取操作、控制操作。  
   操作的对象是二进制数值，他们来自寄存器和指定的内存单元，操作的结果既可以存储在寄存器也可以存储在内存单元。
1. 助记符：二进制码不好记，通常在机器语言中同时使用二进制码和助记符。助记符是一种符号标记。
1. 汇编语言：用助记符表示的语言。相对于二进制，  
   助记符更便于人来使用，执行时还需要翻译为二进制，翻译程序称作汇编编译器。
1. 内存访问：
    1. 算术命令和逻辑命令：不仅允许操控寄存器，而且还可以操控特定的内存单元。
    1. load和store：在寄存器和内存之间传递数据。
1. 寻址方式
    1. 直接寻址：直接表示一个指定内存单元的地址，或者使用一个符号来代表这个指定的地址。
    1. 立即寻址：该方式被用来加载常数，加载那些出现在指令代码中的数值。我们直接将指令数据域中的内容当作要操作的数据装入寄存器，而不是将该数值当作内存单元的地址。
    1. 间接寻址：要访问的内存单元的地址没有直接出现在指令中，而是指令指定的内存单元中的内容代表目标内容单元的地址。

## 五、计算机体系结构

> 1. 图灵机是描述虚拟的简单计算机的抽象机，主要用来分析计算机系统的逻辑基础
> 1. 冯·诺伊曼机是实际应用型的体系结构，它几乎是今天所有计算机的基础。  
     冯·诺依曼结构的基础是一个中央处理单元，它与内存进行交互，负责从输出设备接收数据，向输出设备发送数据。

1. 内存：内存中存有两种不同类型的信息数据项和程序指令，这两种信息通常采用不同的方式来处理，在某些计算机中它们被分别存储在不同的内存区，尽管它们具有不同的功能，都是以二进制的形式存在具有通用结构的随机存储器中。
    1. 数据内存：高级程序操纵抽象的元件，例如变量，数组和对象。
    1. 指令内存：当高级命令被翻译成机器语言时，它变成一系列的二进制，代表机器的指令。
1. 中央处理器：负责执行已被加载到指令内存中的指令。CPU通过三个主要硬件来完成执行。
    1. 蒜素逻辑单元：ALU负责执行计算机中所有底层的算术操作和逻辑操作。
    1. 寄存器：每个CPU都配有一组高速寄存器。减少CPU等待数据的时间。
    1. 控制单元：在指令能够被执行前，需进行解码，指令的解码过程是通过某些控制单元完成的，这些控制单元还负责决定下一步需要取出和执行哪一条指令。
1. 寄存器：位于CPU芯片内部，对它的访问比较快。
    1. 数据寄存器：为CPU提供短期机器服务。
    1. 寻址寄存器：为了进行读写，CPU必须连续访问内存中的数据，这时我们必须确定被访问的内存所在的地址。  
       在某些情况下，这个地址作为当前指令的一部分给出，而其他情况下它依赖前面一条指令的执行结果，此时这个结果应该被存储到某个寄存器中，为后续的操作提供方便。
    1. 程序计数器：执行程序时，CPU必须总是知道下一条指令在指令内存中的地址，这个地址保存在一个特殊的寄存器即程序计数器中。

## 六、汇编编译器

> 机器语言一般分为两类：符号型与二进制型，二进制码代表一条实际的机器指令，它能被底层硬件所理解。  
> 现代计算机平台支持成百上千个这样的基本操作，机器语言就会变得相当复杂，涉及到很多操作码、不同的内存寻址方式和不同的指令格式。
> 解决此复杂性的方法之一是使用约定的语法来表示机器指令。由于将符号表示翻译成二进制码是直截了当的，所以允许用符号表示法来编写底层程序。  
> 符号化的语言称为汇编，翻译程序成为汇编编译器。汇编编译器对每个汇编命令的所有部分进行解析，将每个部分翻译成对应的二进制码。

## 七、虚拟机I：堆栈运算

1. 编译：高级语言程序能都在目标计算机上运行之前，它必须被翻译成计算机的机器语言。这个翻译工作就是编译。  
   通常，必须对任意给定的高级程序和其对应的机器语言编写专用的编译器，每种编译器编译的高级语言与编译之后的机器语言之间存在很强的依赖性。 减少这种依赖性的方法之一是，将整个编译过程划分为两个几乎独立的阶段。
    1. 第一阶段高级程序被解析出来，其命令被翻译成一种中间处理结果。
    1. 第二阶段中间结果被进一步翻译成目标硬件的机器语言。
1. 虚拟机：第一阶段仅依赖于高级语言的细节，第二阶段仅依赖于目标机器语言的细节，这两个阶段之间的接口必须仔细进行设计，甚至将其单独定义为一种抽象的计算机语言 虚拟机。  
   原来作为一个独立的编译器现在被分为两个独立的程序，第一个程序仍称为编译器，将高级代码翻译成中间VM指令，第二个程序将这个VM代码翻译成目标计算机硬件平台的机器语言。

## 八、虚拟机II：程序控制

## 九、高级语言

> 本章介绍了Jack不是为了让你成为Jack程序员，目的是为了今后开发编译器和操作系统奠定基础。

* Jack是简单的基于对象的语言。Jack具有现代语言的基本特性和风格，但是语法相对简单，并且不支持继承。
* 每种编程语言都有一组固定的基本数据类型，Jack支持三种基本数据类型：int，char，boolean。  
  程序员可以通过创建新的抽象数据类型对基本数据类型进行扩展。

## 十、编译器I:语法分析

* 编译器是一种程序，能将高级语言程序从源程序翻译成目标语言。这个翻译过程从概念上讲由两个不同的任务组成。
    1. 语法分析：理解源程序的语法，以此来揭示程序的语义。语法分析通常可以进一步分为两个模块：
        1. 字元化模块：将输入的字符分组成语言原子元素
        2. 语法分析模块：将所得到的语言原子元素集合同语法规则相匹配
    2. 代码生成：
* 我们通常使用一组称为<u>上下文无关语法</u>的规则来描述编程语言。要理解给定的程序，就意味着要决定程序文本和语法规则之间的准确对应。  
  要做到这一点首先必须将程序的文本转换成一系列字元。  
  上下文无关语法是一组规则，用来指定语言中的元素如何由更简单的元素组成。
* 词法分析/字元化：程序最简单的语法形式就是存储在文本文件中的一系列字符。对程序语法分析的第一步就是将字符分组成字元，忽略空格和注释。这一步也称为词法分析、扫描、字元化。  
  字元包括不同的种类，比如while 关键字，count 标识符，< 操作符等，编程语言通常都会指定其所允许的字元类型。
* 语法：词法分析后，形成一系列字元，如何把这些字元分组成变量声明，语句，表达式等语言结构。通过将字元集合按照预定义的规则集即<u>语法</u>进行匹配，就可以实现分组和分类。  
  语法规定字元为终结符，终结符可以组合成为更高级的非终结符，非终结符还可以继续组合成为非终结符。  
  语法是用来执行反向任务的规则：即将给定的输入字元集合解析成非终结符，较低级的非终结符，以及最后不能继续分解的终结符。
* 语法分析：检查语法是否将所输入的文本看作合法输入。

## 十一、编译器II:代码生成

> Jack编译器同Java，C#一样也是基于双层结构的，一个是处于后端的虚拟机，用于把VM语言翻译为机器语言。一个前端模块，把高级语言翻译为VM语言。

> 现代高级编程语言是十分丰富和强大的，他们可以定义和使用大量的数据抽象，也可以实现包含精巧流程控制语句的算法，还可以构建具有任意复杂度的数据结构。  
> 然而，这些程序所最终运行的目标平台是很简单的，一般他们仅提供一组用于存储数据的寄存器，和用于处理数据的原始指令集。

## 十二、操作系统

## 后记
