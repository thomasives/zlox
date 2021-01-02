/// A lox value.  We only handle numbers right now.
pub const Value = f64;

/// Print a `Value` to the writer object
pub fn printValue(writer: anytype, value: Value) !void {
    try writer.print("{d}", .{value});
}