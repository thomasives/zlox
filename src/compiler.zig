const std = @import("std");

const scanner = @import("scanner.zig");

const Allocator = @import("std").mem.Allocator;

pub fn compile(allocator: *Allocator, source_code: []const u8) !void {
    const stdout = std.io.getStdOut().writer();
    scanner.init(source_code);
    
    var line: usize = 0;
    while (true) {
        var token = scanner.nextToken();
        if (token.line != line) {
            try stdout.print("{:0>4} ", .{token.line});
            line = token.line;
        } else {
            try stdout.print("   | ", .{});
        }
        
        try stdout.print("{} '{}'\n", .{token.tag, token.span});
        
        if (token.tag == .token_eof) break;
    }
}
