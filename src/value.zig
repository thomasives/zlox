const std = @import("std");

/// A lox value.  We only handle numbers right now.
pub const Value = union(enum) {
    number: f64,
    boolean: bool,
    nil: void,
    
    pub fn format(
        self: Value,
        comptime fmt: []const u8, 
        options: std.fmt.FormatOptions, 
        writer: anytype
    ) !void {
        switch(self) {
            .number => |v| try writer.print("{d}", .{v}),
            .boolean => |v| {
                var render: []const u8 = if (v) "true" else "false";
                try writer.print("{}", .{render});
            },
            .nil => try writer.print("nil", .{}),
        }
    }
};

pub fn equal(a: Value, b: Value) bool {
    if (@as(@TagType(Value), a) != b) return false;

    switch(a) {
        .boolean => |a_val| return a_val == b.boolean,
        .nil => return true,
        .number => |a_val| return a_val == b.number,
    }
}