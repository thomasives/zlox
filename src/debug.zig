const std = @import("std");

const OpCode = @import("chunk.zig").OpCode;
const Chunk = @import("chunk.zig").Chunk;
const Stack = @import("stack.zig").Stack;
const value = @import("value.zig");

pub const trace_execution = true;
pub const print_code = true;

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

pub fn disassembleInstruction(writer: anytype, chunk: *const Chunk, offset: usize) !usize {
    const op: OpCode = @intToEnum(OpCode, chunk.code.items[offset]);

    switch (op) {
        .op_return => return try simpleInstruction(writer, offset, "OP_RETURN"),
        .op_pop => return try simpleInstruction(writer, offset, "OP_POP"),
        .op_constant => return try constantInstruction(writer, chunk, offset, "OP_CONSTANT"),
        .op_print => return try simpleInstruction(writer, offset, "OP_PRINT"),
        .op_negate => return try simpleInstruction(writer, offset, "OP_NEGATE"),
        .op_add => return try simpleInstruction(writer, offset, "OP_ADD"),
        .op_subtract => return try simpleInstruction(writer, offset, "OP_SUBTRACT"),
        .op_multiply => return try simpleInstruction(writer, offset, "OP_MULTIPLY"),
        .op_divide => return try simpleInstruction(writer, offset, "OP_DIVIDE"),
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

pub fn dumpValueStack(writer: anytype, stack: []value.Value) !void {
    _ = try writer.write("    ");
    for (stack) |v| {
        _ = try writer.write("[ ");
        try value.printValue(writer, v);
        _ = try writer.write(" ]");
    }
    _ = try writer.write("\n");
}


fn expectLine(buffer: []const u8, expected: []const u8) usize {
    const expect = std.testing.expect;
    const eql = std.mem.eql;

    const search_result = std.mem.indexOf(u8, buffer, "\n");
    expect(search_result != null);
    const line_end = search_result.?;
    
    expect(eql(u8, buffer[0..line_end], expected));

    return line_end + 1;
}

test "disassembleChunk op_return" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(OpCode.op_return, 0);
    
    var buffer: [64]u8 = undefined; 
    var writer = std.io.fixedBufferStream(&buffer).writer();
    try disassembleChunk(writer, &chunk, "test chunk");
    
    var line_start: usize = 0;
    line_start += expectLine(buffer[line_start..], "== test chunk ==");
    line_start += expectLine(buffer[line_start..], "0000    0  OP_RETURN       ");
}

test "disassembleChunk op_constant" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const index = try chunk.addConstant(1.2);
    try chunk.writeOp(OpCode.op_constant, 0);
    try chunk.write(index, 0);
    
    var buffer: [64]u8 = undefined; 
    var writer = std.io.fixedBufferStream(&buffer).writer();
    try disassembleChunk(writer, &chunk, "test chunk");
    
    var line_start: usize = 0;
    line_start += expectLine(buffer[line_start..], "== test chunk ==");
    line_start += expectLine(buffer[line_start..], "0000    0  OP_CONSTANT      00 '1.2'");

}

test "disassembleChunk line numbers" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(OpCode.op_return, 0);
    try chunk.writeOp(OpCode.op_return, 0);
    try chunk.writeOp(OpCode.op_return, 0);
    try chunk.writeOp(OpCode.op_return, 1);
    try chunk.writeOp(OpCode.op_return, 1);
    try chunk.writeOp(OpCode.op_return, 2);
    
    var buffer: [256]u8 = undefined; 
    var writer = std.io.fixedBufferStream(&buffer).writer();
    try disassembleChunk(writer, &chunk, "test chunk");
    
    var line_start: usize = 0;
    line_start += expect_line(buffer[line_start..], "== test chunk ==");
    line_start += expect_line(buffer[line_start..], "0000    0  OP_RETURN       ");
    line_start += expect_line(buffer[line_start..], "0001    |  OP_RETURN       ");
    line_start += expect_line(buffer[line_start..], "0002    |  OP_RETURN       ");
    line_start += expect_line(buffer[line_start..], "0003    1  OP_RETURN       ");
    line_start += expect_line(buffer[line_start..], "0004    |  OP_RETURN       ");
    line_start += expect_line(buffer[line_start..], "0005    2  OP_RETURN       ");
}
