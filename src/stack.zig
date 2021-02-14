const std = @import("std");

pub fn Stack(comptime T: type, comptime capacity: usize) type {
    return struct {
        const Self = @This();

        buffer: [capacity]T = undefined,
        top: usize = 0,

        pub fn push(self: *Self, item: T) void {
            std.debug.assert(self.buffer.len > self.top);

            self.buffer[self.top] = item;
            self.top += 1;
        }

        pub fn pop(self: *Self) T {
            std.debug.assert(self.top > 0);

            self.top -= 1;
            return self.buffer[self.top];
        }

        pub fn reset(self: *Self) void {
            self.top = 0;
        }

        pub fn peek(self: *Self, distance: usize) *T {
            std.debug.assert(self.top > 0);

            return &self.buffer[self.top - 1 - distance];
        }

        pub fn full(self: Self) bool {
            return self.top == self.buffer.len;
        }

        pub fn empty(self: Self) bool {
            return self.top == 0;
        }

        pub fn slice(self: Self) []const T {
            return self.buffer[0..self.top];
        }
    };
}

test "stack push" {
    const expect = std.testing.expect;

    var stack: Stack(u8, 16) = .{};
    expect(stack.top == 0);

    stack.push(5);
    expect(stack.top == 1);

    stack.push(6);
    expect(stack.top == 2);
}

test "stack pop" {
    const expect = std.testing.expect;

    var stack: Stack(u8, 16) = .{};

    stack.push(5);
    stack.push(6);
    stack.push(7);

    expect(stack.pop() == 7);
    expect(stack.pop() == 6);
    expect(stack.pop() == 5);
}
