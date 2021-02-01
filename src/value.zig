const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;

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
                        if (function.name) |name| {
                            try writer.print("<fn {}>", .{name.chars});
                        } else {
                            try writer.print("<script>", .{});
                        }
                    },
                    .native => {
                        try writer.print("<native fn>", .{});
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
        native,

        pub fn format(self: Type, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self) {
                .number => try writer.print("{}", .{"number"}),
                .boolean => try writer.print("{}", .{"bool"}),
                .nil => try writer.print("{}", .{"nil"}),
                .string => try writer.print("{}", .{"string"}),
                .function => try writer.print("{}", .{"function"}),
                .native => try writer.print("{}", .{"native fn"}),
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
            *Native => if (self.ty() == .native) {
                return @fieldParentPtr(Native, "base", self.obj);
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
        native,
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
            Native => if (self.ty == .native) {
                return @fieldParentPtr(Native, "base", self);
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
    chunk: Chunk,
    name: ?*String,
};

pub const Native = struct {
    pub const base_type = .native;
    pub const Fn = fn ([]Value) Value;

    base: Obj,
    function: Fn,
};

pub fn equal(a: Value, b: Value) bool {
    if (a.ty() != b.ty()) return false;

    switch (a.ty()) {
        .boolean => return a.boolean == b.boolean,
        .nil => return true,
        .number => return a.number == b.number,
        .string => return a.obj == b.obj,
        .function => return a.obj == b.obj,
        .native => return a.obj == b.obj,
    }
}
