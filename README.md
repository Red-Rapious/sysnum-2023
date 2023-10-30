# Lab1: A net-list simulator
## Compilation and execution

## Behavior of the simulator
### General behavior

### Registers
Variables computed during the current cycle are stored in a hash table (`context`), with the variable identifier as the key. An `environment` hash table is also used to store the values computed during the previous cycle. At the end of each cycle, the environment is replaced by the context, and the context is cleared. 

During the first cycle, if an instruction requests a value from the environment, an arbitrary value of `false` is returned and stored in the hash table.

### The ROM

### The RAM
Each equation in the netlist has its own block of RAM. The RAM is stored in a hash table which is initially empty; its keys are the ident of the corresponding equation, and its values are arrays. For simplicity, the table entries are created the first time that the simulator encounters the equation.

Data is written inside the RAM at the end of each cycle. To do so, another hash table (`ram_to_write`) of the same type is used; when a write instruction is simulated, its data is added to the temporary hash table. At the end of the cycle, the simulator copies all data inside `ram_to_write` to the RAM, and clears `ram_to_write`.

### Bus-wise operations
Binary operators are implemented between two buses. If the two buses are of the same size, the binary operator is applied to each pair of coefficient; otherwise, an error is raised.

## Difficulties encountered
### Debugging
This project showed me how minor mistakes can be hard to spot in a complex architecture. I tried to simplify the debugging process as much as possible by implementing a few debugging tools.

The debugging mode (enabled through the `-dbg` option) shows the step by step execution of the stages of the simulation. At the end of the execution, both the environment and the context are displayed using the `print_environment` function.

Most OCaml errors (such as `Index out of range`) are catched and displayed to the user with additional information (as an example, the simulator will display write address and block size when trying to write at an incorrect position in the RAM).

### Registers

### Handling of the RAM