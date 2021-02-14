const ArrayList = @import("std").ArrayList;
const Allocator = @import("std").mem.Allocator;
const Value = @import("value.zig").Value;

/// One byte op codes for the instructions used in our
/// bytecode interpreter.
pub const OpCode = enum(u8) {
    define_global,
    get_local,
    get_upvalue,
    get_global,
    set_local,
    set_upvalue,
    set_global,
    constant,
    closure,
    pop,
    close_upvalue,
    print,
    call,
    return_,
    negate,
    add,
    subtract,
    multiply,
    divide,
    true_,
    false_,
    nil,
    not,
    equal,
    not_equal,
    greater,
    less,
    greater_equal,
    less_equal,
    jump_if_true,
    jump_if_false,
    jump,
    loop,
};

/// A Chunk of bytecode.  This is the result of compiling a single
/// "Compilation Unit", which in the case of lox is a single function.
pub const Chunk = struct {
    const Self = @This();

    /// the bytecode
    code: Code,
    /// source file line numbers that generated the bytecode
    lines: Lines,
    /// literal values that appeared in the source
    constants: Constants,

    pub const Code = ArrayList(u8);
    pub const Constants = ArrayList(Value);
    pub const Lines = ArrayList(usize);

    pub fn init(allocator: *Allocator) Self {
        return Self{
            .code = Code.init(allocator),
            .lines = Lines.init(allocator),
            .constants = Constants.init(allocator),
        };
    }

    pub fn deinit(self: Self) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    /// Write a single byte to the bytecode that was generated from the
    /// source file at the specified line.
    pub fn write(self: *Self, byte: u8, line: usize) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    /// Write an op code to the bytecode that was generated from the
    /// source file at the specified line.
    pub fn writeOp(self: *Self, opCode: OpCode, line: usize) !void {
        try self.write(@enumToInt(opCode), line);
    }

    /// Add a constant to the constant store.  Returns the index of the
    /// constant in the storage.
    pub fn addConstant(self: *Self, constant: Value) !usize {
        var index = self.constants.items.len;
        try self.constants.append(constant);
        return index;
    }
};
