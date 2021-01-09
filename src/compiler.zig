const std = @import("std");

const scanner = @import("scanner.zig");
const vm = @import("vm.zig");
const debug = @import("debug.zig");
const vl = @import("value.zig");

const Allocator = std.mem.Allocator;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Token = scanner.Token;
const TokenTag = scanner.TokenTag;
const Value = vl.Value;

const Parser = struct {
    current: Token = undefined,
    previous: Token = undefined,
    hadError: bool = false,
    panicMode: bool = false,
};

const Precedence = enum(u8) {
    none,
    assignment,
    or_,
    and_,
    equality,
    comparison,
    term,
    factor,
    unary,
    call,
    primary,
};
const ParseFn = fn () ParseError!void;

const ParseError = anyerror;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,
};

const rules_table = blk: {
    var table = [_]ParseRule{.{
        .prefix = null,
        .infix = null,
        .precedence = .none,
    }} ** @typeInfo(TokenTag).Enum.fields.len;

    table[@enumToInt(TokenTag.left_paren)].prefix = grouping;

    table[@enumToInt(TokenTag.minus)] = ParseRule{
        .prefix = unary,
        .infix = binary,
        .precedence = .term,
    };

    table[@enumToInt(TokenTag.plus)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .term,
    };

    table[@enumToInt(TokenTag.star)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .factor,
    };

    table[@enumToInt(TokenTag.slash)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .factor,
    };

    table[@enumToInt(TokenTag.number)].prefix = number;

    table[@enumToInt(TokenTag.nil)].prefix = literal;
    table[@enumToInt(TokenTag.true_)].prefix = literal;
    table[@enumToInt(TokenTag.false_)].prefix = literal;
    table[@enumToInt(TokenTag.string)].prefix = string;

    table[@enumToInt(TokenTag.bang)].prefix = unary;

    table[@enumToInt(TokenTag.bang_equal)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .equality,
    };

    table[@enumToInt(TokenTag.equal_equal)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .equality,
    };

    table[@enumToInt(TokenTag.less)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .comparison,
    };

    table[@enumToInt(TokenTag.less_equal)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .comparison,
    };

    table[@enumToInt(TokenTag.greater)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .comparison,
    };

    table[@enumToInt(TokenTag.greater_equal)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .comparison,
    };

    break :blk table;
};

fn getRule(tag: TokenTag) *const ParseRule {
    return &rules_table[@enumToInt(tag)];
}

var parser = Parser{};
var compiling_chunk: Chunk = undefined;

pub fn compile(allocator: *Allocator, source_code: []const u8) !Chunk {
    scanner.init(source_code);
    compiling_chunk = Chunk.init(allocator);
    errdefer compiling_chunk.deinit();

    parser.hadError = false;
    parser.panicMode = false;

    try advance();
    try expression();
    try consume(.eof, "Expect end of expression.");

    if (parser.hadError) {
        return vm.InterpretError.Compilation;
    }

    try endCompiler();

    return compiling_chunk;
}

fn parsePrecedence(prec: Precedence) ParseError!void {
    try advance();
    const prefixFn = getRule(parser.previous.tag).prefix;
    if (prefixFn == null) {
        try errorAtPrevious("Expect expression.");
        return;
    }

    try prefixFn.?();

    while (@enumToInt(prec) <= @enumToInt(getRule(parser.current.tag).precedence)) {
        try advance();
        const infixFn = getRule(parser.previous.tag).infix;
        try infixFn.?();
    }
}

fn expression() ParseError!void {
    try parsePrecedence(.assignment);
}

fn literal() ParseError!void {
    switch (parser.previous.tag) {
        .false_ => try emitOp(.false_),
        .true_ => try emitOp(.true_),
        .nil => try emitOp(.nil),
        else => unreachable,
    }
}

fn number() ParseError!void {
    const num = try std.fmt.parseFloat(f64, parser.previous.span);
    const val = Value{ .number = num };
    try emitOpByte(.constant, try makeConstant(val));
}

fn string() ParseError!void {
    const allocator = vm.getAllocator();

    const len = parser.previous.span.len;
    var obj = try vm.createString(parser.previous.span[1 .. len - 1]);

    try emitOpByte(.constant, try makeConstant(Value{ .obj = obj }));
}

fn grouping() ParseError!void {
    try expression();
    try consume(.right_paren, "Expected ')' after expression.");
}

fn unary() ParseError!void {
    const tag = parser.previous.tag;

    try parsePrecedence(.unary);

    switch (tag) {
        .minus => try emitOp(.negate),
        .bang => try emitOp(.not),
        else => unreachable,
    }
}

fn binary() ParseError!void {
    const tag = parser.previous.tag;

    const rule = getRule(tag);
    try parsePrecedence(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));

    switch (tag) {
        .plus => try emitOp(.add),
        .minus => try emitOp(.subtract),
        .star => try emitOp(.multiply),
        .slash => try emitOp(.divide),
        .equal_equal => try emitOp(.equal),
        .bang_equal => try emitOp(.not_equal),
        .less => try emitOp(.less),
        .less_equal => try emitOp(.less_equal),
        .greater => try emitOp(.greater),
        .greater_equal => try emitOp(.greater_equal),
        else => unreachable,
    }
}

fn endCompiler() !void {
    try emitOp(.print);
    try emitOp(.pop);
    try emitOp(.return_);

    if (debug.print_code) {
        const writer = std.io.getStdOut().writer();
        if (!parser.hadError) {
            try debug.disassembleChunk(writer, &compiling_chunk, "code");
        }
    }
}

fn makeConstant(val: Value) !u8 {
    const index = try compiling_chunk.addConstant(val);

    if (index > std.math.maxInt(u8)) {
        try errorAtPrevious("Too many constants for single chunk.");
        return 0;
    }

    return @intCast(u8, index);
}

fn advance() ParseError!void {
    parser.previous = parser.current;

    while (true) {
        parser.current = scanner.nextToken();
        if (parser.current.tag != .error_) break;

        try errorAtCurrent(parser.current.span);
    }
}

fn consume(expected: scanner.TokenTag, error_message: []const u8) !void {
    if (parser.current.tag == expected) {
        try advance();
        return;
    }

    try errorAtCurrent(error_message);
}

fn emitOp(op: OpCode) !void {
    try compiling_chunk.writeOp(op, parser.previous.line);
}

fn emitOpByte(op: OpCode, byte: u8) !void {
    try compiling_chunk.writeOp(op, parser.previous.line);
    try compiling_chunk.write(byte, parser.previous.line);
}

fn errorAtPrevious(message: []const u8) ParseError!void {
    try errorAt(parser.previous, message);
}

fn errorAtCurrent(message: []const u8) ParseError!void {
    try errorAt(parser.current, message);
}

fn errorAt(token: scanner.Token, message: []const u8) ParseError!void {
    if (parser.panicMode) return;
    parser.panicMode = true;

    const stderr = std.io.getStdErr().writer();

    try stderr.print("[line {}] Error ", .{token.line});

    if (token.tag == .eof) {
        try stderr.print("at end", .{});
    } else if (token.tag == .error_) {
        // nothing
    } else {
        try stderr.print("at '{}'", .{token.span});
    }

    try stderr.print(": {}\n", .{message});
    parser.hadError = true;
}
