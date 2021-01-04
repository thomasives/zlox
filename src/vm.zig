const std = @import("std");

const value = @import("value.zig");
const stack = @import("stack.zig");
const debug = @import("debug.zig");
const compile = @import("compiler.zig").compile;

const OpCode = @import("chunk.zig").OpCode;
const Chunk = @import("chunk.zig").Chunk;
const Value = value.Value;
const Allocator = std.mem.Allocator;

pub const InterpretError = error {
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
};

var vm: Vm = undefined;

pub fn init(allocator: *Allocator) !void {
    vm.allocator = allocator;
    vm.value_stack = .{};
}

pub fn deinit() void { }

pub fn interpret(source_code: []const u8) !void {
    std.debug.assert(vm.value_stack.top == 0);
    defer std.debug.assert(vm.value_stack.top == 0);
    
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

fn binaryOp(comptime op: fn (Value, Value) Value) void {
    const b = vm.value_stack.pop();
    const a = vm.value_stack.pop();

    vm.value_stack.push(op(a, b));
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
            .op_return => return,
            .op_pop => _ = vm.value_stack.pop(),
            .op_print => {
                const v = vm.value_stack.buffer[vm.value_stack.top - 1];
                try value.printValue(stdout, v);
                _ = try stdout.write("\n");
            },
            .op_constant => {
                const v = readConstant();
                vm.value_stack.push(v);
            },
            .op_negate => vm.value_stack.push(-vm.value_stack.pop()),
            .op_add => binaryOp(add),
            .op_subtract => binaryOp(subtract),
            .op_multiply => binaryOp(multiply),
            .op_divide => binaryOp(divide),
        }
        
        if (debug.trace_execution) {
            std.debug.assert(next_ip == vm.ip);
        }
    }
}

fn add(a: Value, b: Value) Value {
    return a + b;
}

fn subtract(a: Value, b: Value) Value {
    return a - b;
}

fn multiply(a: Value, b: Value) Value {
    return a * b;
}

fn divide(a: Value, b: Value) Value {
    return a / b;
}