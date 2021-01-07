const std = @import("std");

const value = @import("value.zig");
const stack = @import("stack.zig");
const debug = @import("debug.zig");
const compile = @import("compiler.zig").compile;

const OpCode = @import("chunk.zig").OpCode;
const Chunk = @import("chunk.zig").Chunk;
const Value = value.Value;
const Allocator = std.mem.Allocator;

pub const InterpretError = error{
    Runtime,
    Compilation,
};

const Vm = struct {
    const Self = @This();

    pub const Stack = stack.Stack(Value, 256);

    allocator: *std.mem.Allocator,

    // The currently executing chunk
    chunk: *Chunk,
    // Instruction pointer into the currently executing chunk
    ip: usize,
    value_stack: Stack,

    pub fn peek(self: *Self, distance: usize) Value {
        return self.value_stack.buffer[self.value_stack.top - 1 - distance];
    }
};

var vm: Vm = undefined;

pub fn init(allocator: *Allocator) !void {
    vm.allocator = allocator;
    vm.value_stack = .{};
}

pub fn deinit() void {}

pub fn interpret(source_code: []const u8) !void {
    std.debug.assert(vm.value_stack.top == 0);
    defer std.debug.assert(vm.value_stack.top == 0);
    errdefer vm.value_stack.top = 0;

    var chunk = try compile(vm.allocator, source_code);
    defer chunk.deinit();

    vm.chunk = &chunk;
    vm.ip = 0;

    try run();
}

fn readByte() u8 {
    const result = vm.chunk.code.items[vm.ip];
    vm.ip += 1;
    return result;
}

fn readConstant() Value {
    return vm.chunk.constants.items[readByte()];
}

fn binaryOp(comptime T: type, comptime op: fn (f64, f64) T) !void {
    if (vm.peek(0) != .number or vm.peek(1) != .number) {
        try runtimeError("Operands must be numbers.", .{});
        return InterpretError.Runtime;
    }

    const b = vm.value_stack.pop().number;
    const a = vm.value_stack.pop().number;

    const result = switch (T) {
        f64 => Value{ .number = op(a, b) },
        bool => Value{ .boolean = op(a, b) },
        else => @compileError("Cannot handle type."),
    };

    vm.value_stack.push(result);
}

fn run() !void {
    const stdout = std.io.getStdOut().writer();

    while (true) {
        var next_ip: usize = undefined;
        if (debug.trace_execution) {
            const stack_values = vm.value_stack.buffer[0..vm.value_stack.top];
            try debug.dumpValueStack(stdout, stack_values);
            next_ip = try debug.disassembleInstruction(stdout, vm.chunk, vm.ip);
        }

        const instruction = @intToEnum(OpCode, readByte());
        switch (instruction) {
            .return_ => return,
            .pop => _ = vm.value_stack.pop(),
            .print => {
                const v = vm.peek(0);
                try stdout.print("{}\n", .{v});
            },
            .constant => {
                const v = readConstant();
                vm.value_stack.push(v);
            },
            .negate => {
                if (vm.peek(0) != .number) {
                    try runtimeError("Operand must be a number", .{});
                    return InterpretError.Runtime;
                }

                var operand = vm.value_stack.pop().number;
                var result = Value{ .number = -operand };
                vm.value_stack.push(result);
            },
            .not => {
                var operand = vm.value_stack.pop();
                var result = Value{ .boolean = isFalsey(operand) };
                vm.value_stack.push(result);
            },
            .false_ => vm.value_stack.push(Value{ .boolean = false }),
            .true_ => vm.value_stack.push(Value{ .boolean = true }),
            .nil => vm.value_stack.push(Value.nil),
            .add => try binaryOp(f64, add),
            .subtract => try binaryOp(f64, subtract),
            .multiply => try binaryOp(f64, multiply),
            .divide => try binaryOp(f64, divide),
            .equal => {
                var b = vm.value_stack.pop();
                var a = vm.value_stack.pop();
                vm.value_stack.push(Value{ .boolean = value.equal(a, b) });
            },
            .not_equal => {
                var b = vm.value_stack.pop();
                var a = vm.value_stack.pop();
                vm.value_stack.push(Value{ .boolean = !value.equal(a, b) });
            },
            .greater => try binaryOp(bool, greater),
            .greater_equal => try binaryOp(bool, greaterEqual),
            .less => try binaryOp(bool, less),
            .less_equal => try binaryOp(bool, lessEqual),
        }

        if (debug.trace_execution) {
            std.debug.assert(next_ip == vm.ip);
        }
    }
}

fn runtimeError(comptime fmt: []const u8, args: anytype) !void {
    const stderr = std.io.getStdErr().writer();
    try stderr.print(fmt, args);

    const line = vm.chunk.lines.items[vm.ip];
    try stderr.print("\n[line {}] in script\n", .{line});
}

fn isFalsey(a: Value) bool {
    switch (a) {
        .nil => return true,
        .boolean => |b| return !b,
        else => return false,
    }
}

fn add(a: f64, b: f64) f64 {
    return a + b;
}

fn subtract(a: f64, b: f64) f64 {
    return a - b;
}

fn multiply(a: f64, b: f64) f64 {
    return a * b;
}

fn divide(a: f64, b: f64) f64 {
    return a / b;
}

fn greater(a: f64, b: f64) bool {
    return a > b;
}

fn less(a: f64, b: f64) bool {
    return a < b;
}

fn greaterEqual(a: f64, b: f64) bool {
    return a >= b;
}

fn lessEqual(a: f64, b: f64) bool {
    return a <= b;
}
