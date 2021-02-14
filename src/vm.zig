const std = @import("std");

const vl = @import("value.zig");
const stack = @import("stack.zig");
const debug = @import("debug.zig");

const OpCode = @import("chunk.zig").OpCode;
const Chunk = @import("chunk.zig").Chunk;
const Value = vl.Value;
const Obj = vl.Obj;
const String = vl.String;
const Function = vl.Function;
const Closure = vl.Closure;
const Native = vl.Native;
const Upvalue = vl.Upvalue;
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

const CallFrame = struct {
    closure: *Closure,
    ip: usize,
    slots: []Value,
};

/// The lox virtual machine
const Vm = struct {
    const Self = @This();

    pub const Stack = stack.Stack(Value, 256);
    pub const CallStack = stack.Stack(CallFrame, 64);
    pub const Internment = std.StringHashMap(*Obj);
    pub const Globals = std.AutoHashMap(*String, Value);

    allocator: *std.mem.Allocator,

    /// Stack of function calls
    frames: CallStack,
    /// Runtime stack of values
    stack: Stack,
    /// Global variables
    globals: Globals,
    /// Linked list of open upvalues
    open_upvalues: ?*Upvalue,

    /// Linked list of all objects
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

pub fn newFunction() !*Function {
    const function = try createObj(Function);

    function.arity = 0;
    function.name = null;
    function.chunk = Chunk.init(vm.allocator);
    function.upvalue_count = undefined;

    return function;
}

pub fn newClosure(func: *Function) !*Closure {
    const closure = try createObj(Closure);
    closure.function = func;

    closure.upvalues = try vm.allocator.alloc(?*Upvalue, @intCast(usize, func.upvalue_count));
    for (closure.upvalues) |*upvalue| {
        upvalue.* = null;
    }

    return closure;
}

pub fn newUpvalue(slot: *Value) !*Upvalue {
    const upvalue = try createObj(Upvalue);
    upvalue.location = slot;
    upvalue.next = null;
    upvalue.closed = .nil;

    return upvalue;
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
    vm.allocator.destroy(obj);
}

/// Free the allocated memebers of a managed object
pub fn deallocateObj(obj: *Obj) void {
    switch (obj.ty) {
        .string => {
            const string = obj.cast(String).?;
            vm.allocator.free(string.chars);
        },
        .function => {
            const function = obj.cast(Function).?;
            function.chunk.deinit();
        },
        .closure => {
            const closure = obj.cast(Closure).?;
            vm.allocator.free(closure.upvalues);
        },
        .native,
        .upvalue,
        => {
            // Nothing to do
        },
    }
}

pub fn init(allocator: *Allocator) !void {
    vm.allocator = allocator;
    vm.frames = .{};
    vm.stack = .{};
    vm.open_upvalues = null;
    vm.objects = null;
    vm.strings = Vm.Internment.init(allocator);
    vm.globals = Vm.Globals.init(allocator);

    try defineNative("clock", clockNative);
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

    const func = try compile(vm.allocator, source_code);
    vm.stack.push(Value{ .obj = &func.base });
    const closure = try newClosure(func);
    _ = vm.stack.pop();
    vm.stack.push(Value{ .obj = &closure.base });
    try callValue(Value{ .obj = &closure.base }, 0);

    try run();
}

fn readByte(frame: *CallFrame) u8 {
    const result = frame.closure.function.chunk.code.items[frame.ip];
    frame.ip += 1;
    return result;
}

fn readShort(frame: *CallFrame) u16 {
    var result: u16 = @as(u16, frame.closure.function.chunk.code.items[frame.ip]) << 8;
    result |= frame.closure.function.chunk.code.items[frame.ip + 1];
    frame.ip += 2;
    return result;
}

fn readConstant(frame: *CallFrame) Value {
    return frame.closure.function.chunk.constants.items[readByte(frame)];
}

fn binaryOp(comptime ops: anytype, comptime op_name: []const u8) !void {
    const Dispatcher = struct {
        const CheckFn = fn (Value.Type, Value.Type) bool;
        const ExecuteFn = fn (Value, Value) Op.Error!Value;

        check: CheckFn,
        execute: ExecuteFn,

        fn makeCheck(comptime Ta: type, comptime Tb: type) CheckFn {
            const C = struct {
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

            return C.check;
        }

        fn makeExecute(comptime op: anytype, comptime Ta: type, comptime Tb: type, comptime R: type) ExecuteFn {
            const C = struct {
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

            return C.execute;
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

fn callValue(callee: Value, arg_count: u8) !void {
    if (callee.cast(*Closure)) |closure| {
        const func = closure.function;
        if (arg_count != func.arity) {
            try runtimeError("Expected {} arguments but got {}.", .{ func.arity, arg_count });
            return InterpretError.Runtime;
        }

        if (vm.frames.full()) {
            try runtimeError("Stack overflow.", .{});
            return InterpretError.Runtime;
        }

        vm.frames.push(CallFrame{
            .closure = closure,
            .ip = 0,
            .slots = vm.stack.buffer[vm.stack.top - arg_count - 1 ..],
        });
    } else if (callee.cast(*Native)) |native| {
        const result = native.function(vm.stack.buffer[vm.stack.top - arg_count - 1 ..]);
        vm.stack.top -= arg_count + 1;
        vm.stack.push(result);
    } else {
        try runtimeError("Can only call functions and classes.", .{});
        return InterpretError.Runtime;
    }
}

fn captureUpvalue(local: *Value) !*Upvalue {
    const last = @ptrToInt(local) - @ptrToInt(&vm.stack.buffer[0]);

    var prev: ?*Upvalue = null;
    var next: ?*Upvalue = vm.open_upvalues;

    while (next) |upvalue| {
        var loc = @ptrToInt(upvalue.location) - @ptrToInt(&vm.stack.buffer[0]);
        if (loc / @sizeOf(Upvalue) <= last) break;
        prev = next;
        next = upvalue.next;
    }

    if (next != null and next.?.location == local) {
        return next.?;
    }

    var new = try newUpvalue(local);
    new.next = next;

    if (prev) |upvalue| {
        upvalue.next = new;
    } else {
        vm.open_upvalues = new;
    }

    return new;
}

fn closeUpvalues(last: usize) !void {
    while (vm.open_upvalues) |upvalue| {
        var loc = @ptrToInt(upvalue.location) - @ptrToInt(&vm.stack.buffer[0]);
        if (loc / @sizeOf(Upvalue) < last) break;

        upvalue.closed = upvalue.location.*;
        upvalue.location = &upvalue.closed;
        vm.open_upvalues = upvalue.next;
    }
}

fn run() !void {
    const stdout = std.io.getStdOut().writer();
    var frame = vm.frames.peek(0);

    while (true) {
        if (debug.trace_execution) {
            const top = (@ptrToInt(&vm.stack.buffer[vm.stack.top]) -
                @ptrToInt(&frame.slots[0])) / @sizeOf(Value);
            const stack_values = frame.slots[0..top];
            try stdout.print("  IP = {x:0>4}", .{frame.ip});
            try debug.dumpValueStack(stdout, stack_values);
            _ = try debug.disassembleInstruction(stdout, &frame.closure.function.chunk, frame.ip, false);
        }

        const instruction = @intToEnum(OpCode, readByte(frame));
        switch (instruction) {
            .return_ => {
                const result = vm.stack.pop();
                const new_top = (@ptrToInt(frame.slots.ptr) - @ptrToInt(&vm.stack.buffer[0])) / @sizeOf(Value);

                try closeUpvalues(new_top);

                _ = vm.frames.pop();
                if (vm.frames.empty()) {
                    _ = vm.stack.pop();
                    return;
                }

                vm.stack.top = new_top;
                vm.stack.push(result);

                frame = vm.frames.peek(0);
            },
            .pop => _ = vm.stack.pop(),
            .close_upvalue => {
                try closeUpvalues(vm.stack.top - 1);
                _ = vm.stack.pop();
            },
            .print => {
                const v = vm.stack.pop();
                try stdout.print("{}\n", .{v});
            },
            .constant => {
                const v = readConstant(frame);
                vm.stack.push(v);
            },
            .closure => {
                const function = readConstant(frame).cast(*Function).?;
                const closure = try newClosure(function);
                vm.stack.push(Value{ .obj = &closure.base });
                for (closure.upvalues) |*upvalue| {
                    var is_local = readByte(frame);
                    var index = readByte(frame);
                    if (is_local != 0) {
                        upvalue.* = try captureUpvalue(&frame.slots[index]);
                    } else {
                        upvalue.* = frame.closure.upvalues[index];
                    }
                }
            },
            .define_global => {
                const name = readConstant(frame).cast(*String).?;
                try vm.globals.put(name, vm.stack.pop());
            },
            .get_local => {
                const slot = readByte(frame);
                vm.stack.push(frame.slots[slot]);
            },
            .get_global => {
                const name = readConstant(frame).cast(*String).?;
                const value = vm.globals.get(name);
                if (value) |v| {
                    vm.stack.push(v);
                } else {
                    try runtimeError("Undefined variable '{}'", .{name.chars});
                    return InterpretError.Runtime;
                }
            },
            .set_local => {
                const slot = readByte(frame);
                frame.slots[slot] = vm.stack.peek(0).*;
            },
            .set_global => {
                const name = readConstant(frame).cast(*String).?;
                const entry = vm.globals.getEntry(name);
                if (entry) |e| {
                    e.value = vm.stack.peek(0).*;
                } else {
                    try runtimeError("Undefined variable '{}'", .{name.chars});
                    return InterpretError.Runtime;
                }
            },
            .get_upvalue => {
                var slot = readByte(frame);
                vm.stack.push(frame.closure.upvalues[slot].?.location.*);
            },
            .set_upvalue => {
                var slot = readByte(frame);
                frame.closure.upvalues[slot].?.location.* = vm.stack.peek(0).*;
            },
            .call => {
                const arg_count = readByte(frame);
                try callValue(vm.stack.peek(arg_count).*, arg_count);
                frame = vm.frames.peek(0);
            },
            .negate => {
                if (vm.stack.peek(0).* != .number) {
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
            .jump_if_false => {
                const offset = readShort(frame);
                if (isFalsey(vm.stack.peek(0).*)) {
                    frame.ip += offset;
                }
            },
            .jump_if_true => {
                const offset = readShort(frame);
                if (!isFalsey(vm.stack.peek(0).*)) {
                    frame.ip += offset;
                }
            },
            .jump => {
                const offset = readShort(frame);
                frame.ip += offset;
            },
            .loop => {
                const offset = readShort(frame);
                frame.ip -= offset;
            },
        }
    }
}

fn runtimeError(comptime fmt: []const u8, args: anytype) !void {
    const stderr = std.io.getStdErr().writer();
    try stderr.print(fmt, args);
    try stderr.print("\n", .{});

    var i = @intCast(isize, vm.frames.top) - 1;
    while (i >= 0) : (i -= 1) {
        const frame = vm.frames.peek(@intCast(usize, i));
        const func = frame.closure.function;

        const line = func.chunk.lines.items[frame.ip];

        try stderr.print(
            "[line {}] in {}\n",
            .{ line, if (func.name != null) func.name.?.chars else "script" },
        );
    }

    vm.stack.reset();
    vm.frames.reset();
    vm.open_upvalues = null;
}

fn isFalsey(a: Value) bool {
    switch (a) {
        .nil => return true,
        .boolean => |b| return !b,
        else => return false,
    }
}

fn defineNative(name: []const u8, func: Native.Fn) !void {
    vm.stack.push(Value{ .obj = try createString(name) });
    var native = try createObj(Native);
    native.function = func;
    vm.stack.push(Value{ .obj = &native.base });
    try vm.globals.put(vm.stack.peek(1).cast(*String).?, vm.stack.peek(0).*);
    _ = vm.stack.pop();
    _ = vm.stack.pop();
}

fn clockNative(args: []Value) Value {
    var ts: std.os.timespec = undefined;
    std.os.clock_gettime(std.os.CLOCK_MONOTONIC, &ts) catch |_| {};
    var time = @intToFloat(f64, ts.tv_sec);
    time += @intToFloat(f64, ts.tv_nsec) * 1e-9;
    return Value{ .number = time };
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
