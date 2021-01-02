const std = @import("std");
const debug = @import("debug.zig");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

pub fn main() anyerror!void {
    var allocator = std.testing.allocator;

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    const index = try chunk.addConstant(1.2);
    try chunk.writeOp(OpCode.op_constant, 123);
    try chunk.write(index, 123);
    try chunk.writeOp(OpCode.op_return, 123);

    const stdout = std.io.getStdOut().writer();
    try debug.disassembleChunk(&stdout, &chunk, "test chunk");
}
