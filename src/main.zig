const std = @import("std");
const process = std.process;

const debug = @import("debug.zig");
const vm = @import("vm.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    try vm.init(allocator);
    defer vm.deinit();

    var arg_it = process.args();

    const exe_name = try arg_it.next(allocator).?;
    defer allocator.free(exe_name);

    const path_arg = arg_it.next(allocator);
    if (path_arg == null) {
        try repl(allocator);
    } else {
        const path = try path_arg.?;
        defer allocator.free(path);

        if (arg_it.skip()) {
            std.debug.warn("Usage:\n    {} [path]\n", .{exe_name});
            return error.InvalidArgs;
        }

        try runFile(allocator, path);
    }
}

fn repl(gpa: *Allocator) !void {
    var line_buffer = ArrayList(u8).init(gpa);
    defer line_buffer.deinit();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    while (true) {
        _ = try stdout.write("> ");

        stdin.readUntilDelimiterArrayList(&line_buffer, '\n', 16 * 1024) catch |e| {
            _ = try stdout.write("\n");
            return e;
        };

        if (std.mem.eql(u8, line_buffer.items, "exit")) {
            return;
        }

        vm.interpret(line_buffer.items) catch |e| {
            if (e == vm.InterpretError.Compilation) {
                // TODO
            } else if (e == vm.InterpretError.Runtime) {
                // TODO
            } else {
                return e;
            }
        };
    }
}

fn runFile(gpa: *Allocator, file_path: []const u8) !void {
    var source_code = try readFile(gpa, file_path);
    defer gpa.free(source_code);

    try vm.interpret(source_code);
}

fn readFile(gpa: *Allocator, file_path: []const u8) ![]const u8 {
    var file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();
    var reader = file.reader();

    var source_code = try reader.readAllAlloc(gpa, 1024 * 1024 * 1024);
    return source_code;
}
