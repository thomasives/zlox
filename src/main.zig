const std = @import("std");
const process = std.process;

const debug = @import("debug.zig");
const vm = @import("vm.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

pub fn main() anyerror!void {
    const gpa = std.testing.allocator;

    try vm.init(gpa);
    defer vm.deinit();

    var arg_it = process.args();

    const exe_name = try arg_it.next(gpa).?;
    defer gpa.free(exe_name);

    const path_arg = arg_it.next(gpa);
    if (path_arg == null) {
        try repl(gpa);
    } else {
        const path = try path_arg.?;
        defer gpa.free(path);

        if (arg_it.skip()) {
            std.debug.warn("Usage:\n    {} [path]\n", .{exe_name});
            return error.InvalidArgs;
        }

        try runFile(gpa, path);
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
