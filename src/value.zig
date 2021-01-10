const std = @import("std");

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
                }
            },
        }
    }

    pub const Type = enum {
        number,
        boolean,
        nil,
        string,

        pub fn format(self: Type, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self) {
                .number => try writer.print("{}", .{"number"}),
                .boolean => try writer.print("{}", .{"bool"}),
                .nil => try writer.print("{}", .{"nil"}),
                .string => try writer.print("{}", .{"string"}),
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
            else => unreachable,
        }
    }
};

pub const String = struct {
    pub const base_type = .string;
    base: Obj,
    chars: []const u8,
};

pub fn equal(a: Value, b: Value) bool {
    if (a.ty() != b.ty()) return false;

    switch (a.ty()) {
        .boolean => return a.boolean == b.boolean,
        .nil => return true,
        .number => return a.number == b.number,
        .string => return std.mem.eql(u8, a.cast([]const u8).?, b.cast([]const u8).?),
    }
}
