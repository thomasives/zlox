# zlox 

A (incomplete) zig implementation of the bytecode virtual machine based intepreter for the lox language as described in [Bob Nystrom's Crafting Interpreters](http://craftinginterpreters.com/) web book.

This is an excerise in learning [the zig programming language](https://ziglang.org) and so the code here is probably terrible.  Currently, I am trying to copy the C implementation pretty closely, so there may be some unidiomatic constructions.  I plan to look into how to restructure things so that we can take more advantage of Zig features after I have a working implementation.

Using zig version 0.7.1.

## Progress

- [x] Chapter 14 Chunks of Bytecode
- [x] Chapter 15 A Virtual Machine
- [x] Chapter 16 Scanning on Demand
- [ ] Chapter 17 Compiling Expressions