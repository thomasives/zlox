const std = @import("std");

const OpCode = @import("chunk.zig").OpCode;
const Chunk = @import("chunk.zig").Chunk;
const Stack = @import("stack.zig").Stack;
const value = @import("value.zig");

pub const trace_execution = false;
pub const print_code = true;

fn simpleInstruction(writer: anytype, offset: usize, name: []const u8) !usize {
    try writer.print("{:<16}\n", .{name});

    return offset + 1;
}

fn constantInstruction(writer: anytype, chunk: *const Chunk, offset: usize, name: []const u8) !usize {
    const index = chunk.code.items[offset + 1];

    try writer.print("{:<16} {x:0>2} '{}'\n", .{ name, index, chunk.constants.items[index] });

    return offset + 2;
}

fn byteInstruction(writer: anytype, chunk: *const Chunk, offset: usize, name: []const u8) !usize {
    const slot = chunk.code.items[offset + 1];

    try writer.print("{:<16} {x:0>2}\n", .{ name, slot });

    return offset + 2;
}

pub fn disassembleInstruction(writer: anytype, chunk: *const Chunk, offset: usize) !usize {
    const op: OpCode = @intToEnum(OpCode, chunk.code.items[offset]);

    switch (op) {
        .return_ => return try simpleInstruction(writer, offset, "OP_RETURN"),
        .pop => return try simpleInstruction(writer, offset, "OP_POP"),
        .constant => return try constantInstruction(writer, chunk, offset, "OP_CONSTANT"),
        .print => return try simpleInstruction(writer, offset, "OP_PRINT"),
        .negate => return try simpleInstruction(writer, offset, "OP_NEGATE"),
        .add => return try simpleInstruction(writer, offset, "OP_ADD"),
        .subtract => return try simpleInstruction(writer, offset, "OP_SUBTRACT"),
        .multiply => return try simpleInstruction(writer, offset, "OP_MULTIPLY"),
        .divide => return try simpleInstruction(writer, offset, "OP_DIVIDE"),
        .false_ => return try simpleInstruction(writer, offset, "OP_FALSE"),
        .true_ => return try simpleInstruction(writer, offset, "OP_TRUE"),
        .nil => return try simpleInstruction(writer, offset, "OP_NIL"),
        .not => return try simpleInstruction(writer, offset, "OP_NOT"),
        .equal => return try simpleInstruction(writer, offset, "OP_EQUAL"),
        .not_equal => return try simpleInstruction(writer, offset, "OP_NOT_EQUAL"),
        .less => return try simpleInstruction(writer, offset, "OP_LESS"),
        .less_equal => return try simpleInstruction(writer, offset, "OP_LESS_EQUAL"),
        .greater => return try simpleInstruction(writer, offset, "OP_GREATER"),
        .greater_equal => return try simpleInstruction(writer, offset, "OP_GREATER_EQUAL"),
        .define_global => return constantInstruction(writer, chunk, offset, "OP_DEFINE_GLOBAL"),
        .get_global => return constantInstruction(writer, chunk, offset, "OP_GET_GLOBAL"),
        .get_local => return byteInstruction(writer, chunk, offset, "OP_GET_LOCAL"),
        .set_global => return constantInstruction(writer, chunk, offset, "OP_SET_GLOBAL"),
        .set_local => return byteInstruction(writer, chunk, offset, "OP_SET_LOCAL"),
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
        try writer.print("[ {} ]", .{v});
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
