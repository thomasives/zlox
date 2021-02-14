const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;

fn printFunction(writer: anytype, function: *Function) !void {
    if (function.name) |name| {
        try writer.print("<fn {}>", .{name.chars});
    } else {
        try writer.print("<script>", .{});
    }
}

/// A lox value.
pub const Value = union(enum) {
    number: f64,
    boolean: bool,
    nil: void,
    obj: *Obj,

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .number => |v| try writer.print("{d}", .{v}),
            .boolean => |v| {
                var rendering: []const u8 = if (v) "true" else "false";
                try writer.print("{}", .{rendering});
            },
            .nil => try writer.print("nil", .{}),
            .obj => {
                switch (self.obj.ty) {
                    .string => try writer.print("\"{}\"", .{self.cast([]const u8).?}),
                    .function => {
                        const function = self.cast(*Function).?;
                        try printFunction(writer, function);
                    },
                    .closure => {
                        const closure = self.cast(*Closure).?;
                        try printFunction(writer, closure.function);
                    },
                    .native => {
                        try writer.print("<native fn>", .{});
                    },
                    .upvalue => {
                        try writer.print("<upvalue>", .{});
                    },
                }
            },
        }
    }

    pub const Type = enum {
        number,
        boolean,
        nil,
        string,
        function,
        closure,
        native,
        upvalue,

        pub fn format(self: Type, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self) {
                .number => try writer.print("{}", .{"number"}),
                .boolean => try writer.print("{}", .{"bool"}),
                .nil => try writer.print("{}", .{"nil"}),
                .string => try writer.print("{}", .{"string"}),
                .function => try writer.print("{}", .{"function"}),
                .closure => try writer.print("{}", .{"closure"}),
                .native => try writer.print("{}", .{"native fn"}),
                .upvalue => try writer.print("{}", .{"upvalue"}),
            }
        }
    };

    pub fn ty(self: Value) Type {
        switch (self) {
            .number => return .number,
            .boolean => return .boolean,
            .nil => return .nil,
            .obj => {
                switch (self.obj.ty) {
                    .string => return .string,
                    .function => return .function,
                    .native => return .native,
                    .closure => return .closure,
                    .upvalue => return .upvalue,
                }
            },
        }
    }

    pub fn cast(self: Value, comptime T: type) ?T {
        switch (T) {
            f64 => if (self == .number) {
                return self.number;
            } else {
                return null;
            },
            bool => if (self == .boolean) {
                return self.boolean;
            } else {
                return null;
            },
            *Obj => if (self == .obj) {
                return self.obj;
            } else {
                return null;
            },
            *String => if (self.ty() == .string) {
                return @fieldParentPtr(String, "base", self.obj);
            } else {
                return null;
            },
            *Function => if (self.ty() == .function) {
                return @fieldParentPtr(Function, "base", self.obj);
            } else {
                return null;
            },
            *Closure => if (self.ty() == .closure) {
                return @fieldParentPtr(Closure, "base", self.obj);
            } else {
                return null;
            },
            *Native => if (self.ty() == .native) {
                return @fieldParentPtr(Native, "base", self.obj);
            } else {
                return null;
            },
            *Upvalue => if (self.ty() == .upvalue) {
                return @fieldParentPtr(Upvalue, "base", self.obj);
            } else {
                return null;
            },
            []const u8 => if (self.ty() == .string) {
                return @fieldParentPtr(String, "base", self.obj).chars;
            } else {
                return null;
            },
            else => unreachable,
        }
    }
};

pub const Obj = struct {
    pub const Type = enum {
        string,
        function,
        closure,
        native,
        upvalue,
    };
    ty: Type,
    next: ?*Obj,

    pub fn cast(self: *Obj, comptime T: type) ?*T {
        switch (T) {
            String => if (self.ty == .string) {
                return @fieldParentPtr(String, "base", self);
            } else {
                return null;
            },
            Function => if (self.ty == .function) {
                return @fieldParentPtr(Function, "base", self);
            } else {
                return null;
            },
            Closure => if (self.ty == .closure) {
                return @fieldParentPtr(Closure, "base", self);
            } else {
                return null;
            },
            Native => if (self.ty == .native) {
                return @fieldParentPtr(Native, "base", self);
            } else {
                return null;
            },
            Upvalue => if (self.ty == .upvalue) {
                return @fieldParentPtr(Upvalue, "base", self);
            } else {
                return null;
            },
            else => unreachable,
        }
    }
};

pub const String = struct {
    pub const base_type = .string;
    base: Obj,
    chars: []const u8,
};

pub const Function = struct {
    pub const base_type = .function;
    base: Obj,
    arity: i32,
    upvalue_count: i32,
    chunk: Chunk,
    name: ?*String,
};

pub const Closure = struct {
    pub const base_type = .closure;
    base: Obj,
    function: *Function,
    upvalues: []?*Upvalue
};

pub const Native = struct {
    pub const base_type = .native;
    pub const Fn = fn ([]Value) Value;

    base: Obj,
    function: Fn,
};

pub const Upvalue = struct {
    pub const base_type = .upvalue;
    base: Obj,
    next: ?*Upvalue,
    location: *Value,
    closed: Value,
};

pub fn equal(a: Value, b: Value) bool {
    if (a.ty() != b.ty()) return false;

    switch (a.ty()) {
        .boolean => return a.boolean == b.boolean,
        .nil => return true,
        .number => return a.number == b.number,
        .string,
        .function,
        .native,
        .closure,
        .upvalue,
        => return a.obj == b.obj,
    }
}
