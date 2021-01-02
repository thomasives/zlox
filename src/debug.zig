const std = @import("std");

const OpCode = @import("chunk.zig").OpCode;
const Chunk = @import("chunk.zig").Chunk;
const value = @import("value.zig");

// XXX(tri): @Investigate Is there a better way of passing the writer object
// rather than using anytype?  

fn simpleInstruction(writer: anytype, offset: usize, name: []const u8) !usize {
    try writer.print("{:<16}\n", .{name});

    return offset + 1;
}

fn constantInstruction(writer: anytype, chunk: *const Chunk, offset: usize, name: []const u8) !usize {
    const index = chunk.code.items[offset + 1];

    try writer.print("{:<16} {x:0<2} '", .{ name, index });
    try value.printValue(writer, chunk.constants.items[index]);
    try writer.print("'\n", .{});

    return offset + 2;
}

fn disassembleInstruction(writer: anytype, chunk: *const Chunk, offset: usize) !usize {
    const op: OpCode = @intToEnum(OpCode, chunk.code.items[offset]);

    switch (op) {
        OpCode.op_return => return try simpleInstruction(writer, offset, "OP_RETURN"),
        OpCode.op_constant => return try constantInstruction(writer, chunk, offset, "OP_CONSTANT"),
    }
}

/// Disassemble a chunk of code, writing the output to the writer object.
pub fn disassembleChunk(writer: anytype, chunk: *const Chunk, name: []const u8) !void {
    try writer.print("== {} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        try writer.print("{x:0>4} ", .{offset});

        var line_number = chunk.lines.items[offset];
        var is_new_line = offset == 0 or line_number != chunk.lines.items[offset - 1];

        if (is_new_line) {
            try writer.print("{:4}  ", .{line_number});
        } else {
            try writer.print("   |  ", .{});
        }

        offset = try disassembleInstruction(writer, chunk, offset);
    }
}
