const std = @import("std");

const vl = @import("value.zig");
const stack = @import("stack.zig");
const debug = @import("debug.zig");

const OpCode = @import("chunk.zig").OpCode;
const Chunk = @import("chunk.zig").Chunk;
const Value = vl.Value;
const Obj = vl.Obj;
const String = vl.String;
const Allocator = std.mem.Allocator;

const compile = @import("compiler.zig").compile;

// TODO remove me
// I think we possibly want an "ObjStore" object to manage this stuff
pub fn getAllocator() *Allocator {
    return vm.allocator;
}

// TODO sort out error handling
pub const InterpretError = error{
    Runtime,
    Compilation,
};

/// The lox virtual machine
const Vm = struct {
    const Self = @This();

    pub const Stack = stack.Stack(Value, 256);
    pub const Internment = std.StringHashMap(*Obj);
    pub const Globals = std.AutoHashMap(*String, Value);

    allocator: *std.mem.Allocator,

    /// The currently executing chunk
    chunk: *Chunk,
    /// Instruction pointer into the currently executing chunk
    ip: usize,
    /// Runtime stack of values
    stack: Stack,
    /// Global variables
    globals: Globals,

    /// Linked list of objects
    objects: ?*Obj,
    /// Internment camp for all the strings in the lox program
    strings: Internment,
};

var vm: Vm = undefined;

/// Create an object managed by the VM.
pub fn createObj(comptime T: type) !*T {
    var ptr = try vm.allocator.create(T);

    ptr.base = Obj{
        .ty = T.base_type,
        .next = vm.objects,
    };
    vm.objects = &ptr.base;

    return ptr;
}

pub fn createString(chars: []const u8) !*Obj {
    var interned = vm.strings.get(chars);

    if (interned != null) {
        return interned.?;
    } else {
        var duped = try vm.allocator.dupe(u8, chars);
        errdefer vm.allocator.free(duped);

        var obj = try createStringNoDupe(duped);
        try vm.strings.putNoClobber(duped, obj);

        return obj;
    }
}

fn createStringNoDupe(chars: []const u8) !*Obj {
    var obj = try createObj(String);
    obj.chars = chars;
    return &obj.base;
}

/// Free an object managed by the VM
pub fn destroyObj(obj: *Obj) void {
    switch (obj.ty) {
        .string => {
            const string = obj.cast(String).?;
            vm.allocator.destroy(obj);
        },
    }
}

/// Free the allocated memebers of a managed object
pub fn deallocateObj(obj: *Obj) void {
    switch (obj.ty) {
        .string => {
            const string = obj.cast(String).?;
            vm.allocator.free(string.chars);
        },
    }
}

pub fn init(allocator: *Allocator) !void {
    vm.allocator = allocator;
    vm.stack = .{};
    vm.objects = null;
    vm.strings = Vm.Internment.init(allocator);
    vm.globals = Vm.Globals.init(allocator);
}

pub fn deinit() void {
    vm.strings.deinit();
    vm.globals.deinit();
    var obj = vm.objects;
    while (obj != null) {
        var next = obj.?.next;
        deallocateObj(obj.?);
        destroyObj(obj.?);
        obj = next;
    }
}

pub fn interpret(source_code: []const u8) !void {
    std.debug.assert(vm.stack.top == 0);
    defer std.debug.assert(vm.stack.top == 0);
    errdefer vm.stack.top = 0;

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

fn binaryOp(comptime ops: anytype, comptime op_name: []const u8) !void {
    const Dispatcher = struct {
        const CheckFn = fn (Value.Type, Value.Type) bool;
        const ExecuteFn = fn (Value, Value) Op.Error!Value;

        check: CheckFn,
        execute: ExecuteFn,

        fn makeCheck(comptime Ta: type, comptime Tb: type) CheckFn {
            const Closure = struct {
                fn check(a: Value.Type, b: Value.Type) bool {
                    switch (Ta) {
                        f64 => if (a != .number) return false,
                        bool => if (a != .boolean) return false,
                        []const u8 => if (a != .string) return false,
                        else => unreachable,
                    }

                    switch (Tb) {
                        f64 => return b == .number,
                        bool => return b == .boolean,
                        []const u8 => return b == .string,
                        else => unreachable,
                    }
                }
            };

            return Closure.check;
        }

        fn makeExecute(comptime op: anytype, comptime Ta: type, comptime Tb: type, comptime R: type) ExecuteFn {
            const Closure = struct {
                fn execute(a: Value, b: Value) Op.Error!Value {
                    const a_inner = a.cast(Ta).?;
                    const b_inner = b.cast(Tb).?;

                    switch (R) {
                        f64 => return Value{ .number = try op(a_inner, b_inner) },
                        bool => return Value{ .boolean = try op(a_inner, b_inner) },
                        []const u8 => {
                            const chars = try op(a_inner, b_inner);
                            errdefer vm.allocator.free(chars);

                            var interned = vm.strings.get(chars);
                            if (interned != null) {
                                vm.allocator.free(chars);
                                return Value{ .obj = interned.? };
                            } else {
                                var obj = try createStringNoDupe(chars);
                                try vm.strings.putNoClobber(chars, obj);

                                return Value{ .obj = obj };
                            }
                        },
                        else => unreachable,
                    }
                }
            };

            return Closure.execute;
        }
    };

    const ops_info = @typeInfo(@TypeOf(ops)).Struct;

    const dispatchers = blk: {
        var disps: [ops_info.fields.len]Dispatcher = undefined;
        inline for (ops) |op, i| {
            const fn_info = @typeInfo(@TypeOf(op)).Fn;
            const args = fn_info.args;

            const Ta = args[0].arg_type.?;
            const Tb = args[1].arg_type.?;
            const R = @typeInfo(fn_info.return_type.?).ErrorUnion.payload;

            disps[i] = .{
                .check = Dispatcher.makeCheck(Ta, Tb),
                .execute = Dispatcher.makeExecute(op, Ta, Tb, R),
            };
        }
        break :blk disps;
    };

    const b = vm.stack.pop();
    const a = vm.stack.pop();

    for (dispatchers) |d| {
        if (d.check(a.ty(), b.ty())) {
            vm.stack.push(try d.execute(a, b));
            return;
        }
    }

    try runtimeError("Binary operator '{}' not supported for types '{}' and '{}'", .{
        op_name,
        a.ty(),
        b.ty(),
    });
    return InterpretError.Runtime;
}

fn run() !void {
    const stdout = std.io.getStdOut().writer();

    while (true) {
        var next_ip: usize = undefined;
        if (debug.trace_execution) {
            const stack_values = vm.stack.buffer[0..vm.stack.top];
            try debug.dumpValueStack(stdout, stack_values);
            next_ip = try debug.disassembleInstruction(stdout, vm.chunk, vm.ip);
        }

        const instruction = @intToEnum(OpCode, readByte());
        switch (instruction) {
            .return_ => return,
            .pop => _ = vm.stack.pop(),
            .print => {
                const v = vm.stack.pop();
                try stdout.print("{}\n", .{v});
            },
            .constant => {
                const v = readConstant();
                vm.stack.push(v);
            },
            .define_global => {
                const name = readConstant().cast(*String).?;
                try vm.globals.put(name, vm.stack.pop());
            },
            .get_global => {
                const name = readConstant().cast(*String).?;
                const value = vm.globals.get(name);
                if (value) |v| {
                    vm.stack.push(v);
                } else {
                    try runtimeError("Undefined variable '{}'", .{name.chars});
                    return InterpretError.Runtime;
                }
            },
            .set_global => {
                const name = readConstant().cast(*String).?;
                const entry = vm.globals.getEntry(name);
                if (entry) |e| {
                    e.value = vm.stack.peek(0);
                } else {
                    try runtimeError("Undefined variable '{}'", .{name.chars});
                    return InterpretError.Runtime;
                }
            },
            .negate => {
                if (vm.stack.peek(0) != .number) {
                    try runtimeError("Operand must be a number", .{});
                    return InterpretError.Runtime;
                }

                var operand = vm.stack.pop().number;
                var result = Value{ .number = -operand };
                vm.stack.push(result);
            },
            .not => {
                var operand = vm.stack.pop();
                var result = Value{ .boolean = isFalsey(operand) };
                vm.stack.push(result);
            },
            .false_ => vm.stack.push(Value{ .boolean = false }),
            .true_ => vm.stack.push(Value{ .boolean = true }),
            .nil => vm.stack.push(Value.nil),
            .add => try binaryOp(.{ Op.add, Op.concat }, "+"),
            .subtract => try binaryOp(.{Op.subtract}, "-"),
            .multiply => try binaryOp(.{Op.multiply}, "*"),
            .divide => try binaryOp(.{Op.divide}, "/"),
            .equal => {
                var b = vm.stack.pop();
                var a = vm.stack.pop();
                vm.stack.push(Value{ .boolean = vl.equal(a, b) });
            },
            .not_equal => {
                var b = vm.stack.pop();
                var a = vm.stack.pop();
                vm.stack.push(Value{ .boolean = !vl.equal(a, b) });
            },
            .greater => try binaryOp(.{Op.greater}, ">"),
            .greater_equal => try binaryOp(.{Op.greaterEqual}, ">="),
            .less => try binaryOp(.{Op.less}, "<"),
            .less_equal => try binaryOp(.{Op.lessEqual}, "<="),
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

const Op = struct {
    const Error = error{OutOfMemory};

    fn concat(a: []const u8, b: []const u8) Error![]const u8 {
        return try std.mem.concat(
            vm.allocator,
            u8,
            &[_][]const u8{ a, b },
        );
    }

    fn add(a: f64, b: f64) Error!f64 {
        return a + b;
    }

    fn subtract(a: f64, b: f64) Error!f64 {
        return a - b;
    }

    fn multiply(a: f64, b: f64) Error!f64 {
        return a * b;
    }

    fn divide(a: f64, b: f64) Error!f64 {
        return a / b;
    }

    fn greater(a: f64, b: f64) Error!bool {
        return a > b;
    }

    fn less(a: f64, b: f64) Error!bool {
        return a < b;
    }

    fn greaterEqual(a: f64, b: f64) Error!bool {
        return a >= b;
    }

    fn lessEqual(a: f64, b: f64) Error!bool {
        return a <= b;
    }
};
