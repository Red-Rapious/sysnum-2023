# Lab1: A net-list simulator
> Note: the source code for this project is available [on GitHub](https://github.com/Red-Rapious/sysnum-2023).

## Compilation and execution
You can compile my code with:
```
ocamlbuild netlist_simulator.byte
```
Then, you can simulate an example using:
```
./netlist_simulator.byte [options] test/fulladder.net
```

The following options are available:
```
-n <n>        Number of steps to simulate
-print_only   Print the sorted net-list on standard output without simulating it
-dbg          Enable the debug mode, with more informations being displayed
-rom          Name of the file used for ROM. If not specified, will load ROM.txt
-help         Display the list of options
```

## Behavior of the simulator
### Registers
Variables computed during the current cycle are stored in a hash table (`context`), with the variable identifier as the key. An `environment` hash table is also used to store the values computed during the *previous* cycle. At the end of each cycle, the environment is replaced by the context, and the context is cleared. 

During the first cycle, if an instruction requests a value from the environment, an arbitrary value of `false` is returned and stored in the hash table.

### The RAM
Each equation in the netlist has its own block of RAM. The RAM is stored in a hash table, `ram`, which is initially empty; its keys are the identifier of the corresponding equation, and its values are arrays. For simplicity, the table entries are created the first time that the simulator encounters the equation.

Data is written inside the RAM at the end of each cycle. To do so, another hash table (`ram_to_write`) of the same type is used; when a write instruction is simulated, its data is added to the temporary hash table. At the end of the cycle, the simulator copies all data inside `ram_to_write` to the RAM (the `ram` hash table), and clears `ram_to_write`.

Note that the only parameter of the `RAM` instruction that introduces an edge in the dependency graph of the scheduler is `read_addr`. Others are not needed to *compute the value*.

### The ROM
The ROM is loaded from an external file. The default file name is `ROM.txt`, but the `-rom` option allow the user to specify another file. The simulator will read the first line of the file, and load it as an array named `rom`. `ROM` instructions will simply read a part of the array. You can test the ROM using the `test/rom.net` file.

### Bus-wise operations
Binary operators are implemented between two buses. If the two buses are of the same size, the binary operator is applied to each pair of booleans; otherwise, an error is raised. Likewise, operations between a bus and a bit are not permitted.

## Difficulties encountered
### Handling of memory
I did not immediatly realise the need of two hash tables (`context` and `environment`) to store the variables. I had to debug my simulator before adding `environment`. Similarly, the implementation of ROM and RAM were not intuitive and required discussions with other students to understand what was the best way to handle it.

### Debugging
This project showed me how minor mistakes can be hard to spot in a complex architecture. I tried to simplify the debugging process as much as possible by implementing a few debugging tools.

The debugging mode (enabled through the `-dbg` option) shows the step by step execution of the stages of the simulation. At the end of the execution, both the environment and the context are displayed using the `print_environment` function.

Most OCaml errors (such as `Index out of range`) are catched and displayed to the user with additional information (as an example, the simulator will display write address and block size when trying to write at an incorrect position in the RAM).