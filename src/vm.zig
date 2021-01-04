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

    try compile(vm.allocator, source_code);
}

fn readByte() u8 {
    const result = vm.chunk.code.items[self.ip]; 
    self.ip += 1;
    return result;
}

fn readConstant(self: *Self) Value {
    return self.chunk.constants.items[self.readByte()];
}

fn binaryOp(self: *Self, comptime op: fn (Value, Value) Value) void {
    const b = self.value_stack.pop();
    const a = self.value_stack.pop();

    self.value_stack.push(op(a, b));
}

fn run(self: *Self) !void {
    const stdout = std.io.getStdOut().writer();

    while (true) {
        var next_ip: usize = undefined;
        if (debug.trace_execution) {
            const stack_values = self.value_stack.buffer[0..self.value_stack.top];
            try debug.dumpValueStack(stdout, stack_values);
            next_ip = try debug.disassembleInstruction(stdout, self.chunk, self.ip);
        }

        const instruction = @intToEnum(OpCode, self.readByte());
        switch (instruction) {
            .op_return => return,
            .op_pop => _ = self.value_stack.pop(),
            .op_print => {
                const v = self.value_stack.buffer[self.value_stack.top - 1];
                try value.printValue(stdout, v);
                _ = try stdout.write("\n");
            },
            .op_constant => {
                const v = self.readConstant();
                self.value_stack.push(v);
            },
            .op_negate => self.value_stack.push(-self.value_stack.pop()),
            .op_add => self.binaryOp(add),
            .op_subtract => self.binaryOp(subtract),
            .op_multiply => self.binaryOp(multiply),
            .op_divide => self.binaryOp(divide),
        }
        
        if (debug.trace_execution) {
            std.debug.assert(next_ip == self.ip);
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