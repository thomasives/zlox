const std = @import("std");

pub fn FixedCapacityStack(comptime T: type, comptime capacity: usize) type {
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
    };
}

test "stack push" {
    const expect = std.testing.expect;

    var stack: FixedCapacityStack(u8, 16) = .{};
    expect(stack.top == 0);

    stack.push(5);
    expect(stack.top == 1);

    stack.push(6);
    expect(stack.top == 2);
}

test "stack pop" {
    const expect = std.testing.expect;

    var stack: FixedCapacityStack(u8, 16) = .{};

    stack.push(5);
    stack.push(6);
    stack.push(7);
    
    expect(stack.pop() == 7);
    expect(stack.pop() == 6);
    expect(stack.pop() == 5);
}