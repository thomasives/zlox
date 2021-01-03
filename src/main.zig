const std = @import("std");
const debug = @import("debug.zig");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Vm = @import("vm.zig").Vm;

pub fn main() anyerror!void {
    const allocator = std.testing.allocator;

    var vm = Vm.init();
    defer vm.deinit();

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    {
        const index = try chunk.addConstant(5.0);
        try chunk.writeOp(OpCode.op_constant, 123);
        try chunk.write(index, 123);
    }
    
    {
        const index = try chunk.addConstant(8.0);
        try chunk.writeOp(OpCode.op_constant, 123);
        try chunk.write(index, 123);
    }

    {
        const index = try chunk.addConstant(2.0);
        try chunk.writeOp(OpCode.op_constant, 123);
        try chunk.write(index, 123);
    }

    try chunk.writeOp(OpCode.op_negate, 123);
    try chunk.writeOp(OpCode.op_add, 123);
    try chunk.writeOp(OpCode.op_multiply, 123);

    try chunk.writeOp(OpCode.op_print, 123);
    try chunk.writeOp(OpCode.op_pop, 123);
    try chunk.writeOp(OpCode.op_return, 123);
    
    const stdout = std.io.getStdOut().writer();
    try debug.disassembleChunk(&stdout, &chunk, "test chunk");
    _ = try stdout.write("\n");

    try vm.interpret(stdout, &chunk);
}
