const std = @import("std");

const OpCode = @import("chunk.zig").OpCode;
const Chunk = @import("chunk.zig").Chunk;
const value = @import("value.zig");
const Value = value.Value;
const stack = @import("stack.zig");

const debug = @import("debug.zig");

pub const Vm = struct {
    const Self = @This();
    
    pub const Stack = stack.Stack(Value, 256);

    // The currently executing chunk
    chunk: *Chunk,  
    // Instruction pointer into the currently executing chunk
    ip: usize,
    value_stack: Stack,

    pub const InterpretError = error {
        Runtime,
        Compilation,
    };
    
    pub fn init() Self {
        return Self {
            .chunk = undefined,
            .ip = undefined,
            .value_stack = .{}
        };
    }
    
    pub fn deinit(self: *Self) void { }
    
    pub fn interpret(self: *Self, writer: anytype, chunk: *Chunk) !void {
        std.debug.assert(self.value_stack.top == 0);
        defer std.debug.assert(self.value_stack.top == 0);

        self.chunk = chunk;
        self.ip = 0;
        
        return try self.run(writer);
    }
    
    fn readByte(self: *Self) u8 {
        const result = self.chunk.code.items[self.ip]; 
        self.ip += 1;
        return result;
    }
    
    fn readConstant(self: *Self) Value {
        return self.chunk.constants.items[self.readByte()];
    }
    
    fn binary_op(self: *Self, comptime op: fn (Value, Value) Value) void {
        const b = self.value_stack.pop();
        const a = self.value_stack.pop();

        self.value_stack.push(op(a, b));
    }
    
    fn run(self: *Self, writer: anytype) !void {
        while (true) {
            var next_ip: usize = undefined;
            if (debug.trace_execution) {
                const stack_values = self.value_stack.buffer[0..self.value_stack.top];
                try debug.dumpValueStack(writer, stack_values);
                next_ip = try debug.disassembleInstruction(writer, self.chunk, self.ip);
            }

            const instruction = @intToEnum(OpCode, self.readByte());
            switch (instruction) {
                .op_return => return,
                .op_pop => _ = self.value_stack.pop(),
                .op_print => {
                    const v = self.value_stack.buffer[self.value_stack.top - 1];
                    try value.printValue(writer, v);
                    _ = try writer.write("\n");
                },
                .op_constant => {
                    const v = self.readConstant();
                    self.value_stack.push(v);
                },
                .op_negate => self.value_stack.push(-self.value_stack.pop()),
                .op_add => self.binary_op(add),
                .op_subtract => self.binary_op(subtract),
                .op_multiply => self.binary_op(multiply),
                .op_divide => self.binary_op(divide),
            }
            
            if (debug.trace_execution) {
                std.debug.assert(next_ip == self.ip);
            }
        }
    }
};

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