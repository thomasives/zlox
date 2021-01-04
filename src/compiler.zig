const std = @import("std");

const scanner = @import("scanner.zig");
const vm = @import("vm.zig");
const debug = @import("debug.zig");

const Allocator = std.mem.Allocator;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Token = scanner.Token;
const TokenTag = scanner.TokenTag;

const Parser = struct {
    current: Token = undefined,
    previous: Token = undefined,
    hadError: bool = false,
    panicMode: bool = false,
};

const Precedence = enum(u8) {
    prec_none,
    prec_assignment,
    prec_or,
    prec_and,
    prec_equality,
    prec_comparison,
    prec_term,
    prec_factor,
    prec_unary,
    prec_call,
    prec_primary,
};
const ParseFn = fn () ParseError!void;

const ParseError = anyerror;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: u8,
};

const rule_table_size = @typeInfo(TokenTag).Enum.fields.len;
const rules_table: [rule_table_size]ParseRule = comptime buildRulesTable(); 

fn buildRulesTable() [rule_table_size]ParseRule {
    var table: [rule_table_size]ParseRule = undefined;

    for (table) |*rule| {
        rule.* = ParseRule {
            .prefix = null,
            .infix = null,
            .precedence = @enumToInt(Precedence.prec_none),
        };
    }

    table[@enumToInt(TokenTag.token_left_paren)].prefix = grouping;

    table[@enumToInt(TokenTag.token_minus)] = ParseRule {
        .prefix = unary, 
        .infix = binary,
        .precedence = @enumToInt(Precedence.prec_term),
     };

    table[@enumToInt(TokenTag.token_plus)] = ParseRule {
        .prefix = null, 
        .infix = binary,
        .precedence = @enumToInt(Precedence.prec_term),
     };

    table[@enumToInt(TokenTag.token_star)] = ParseRule {
        .prefix = null, 
        .infix = binary,
        .precedence = @enumToInt(Precedence.prec_factor),
     };

    table[@enumToInt(TokenTag.token_slash)] = ParseRule {
        .prefix = null, 
        .infix = binary,
        .precedence = @enumToInt(Precedence.prec_factor),
     };

    table[@enumToInt(TokenTag.token_number)].prefix = number;
    
    return table;
}

fn getRule(tag: TokenTag) *const ParseRule {
    return &rules_table[@enumToInt(tag)];
}

var parser = Parser { };
var compiling_chunk: Chunk = undefined;

pub fn compile(allocator: *Allocator, source_code: []const u8) !Chunk {
    scanner.init(source_code);
    compiling_chunk = Chunk.init(allocator);
    errdefer compiling_chunk.deinit();
    
    parser.hadError = false;
    parser.panicMode = false;

    try advance();
    try expression();
    try consume(.token_eof, "Expect end of expression.");
    
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
    
    while (@enumToInt(prec) <= getRule(parser.current.tag).precedence) {
        try advance();
        const infixFn = getRule(parser.previous.tag).infix;
        try infixFn.?();
    }
}

fn expression() ParseError!void {
    try parsePrecedence(.prec_assignment);
}

fn number() ParseError!void {
    const value = try std.fmt.parseFloat(f64, parser.previous.span);
    try emitOpByte(.op_constant, try makeConstant(value));
}

fn grouping() ParseError!void {
    try expression();
    try consume(.token_right_paren, "Expected ')' after expression.");
}

fn unary() ParseError!void {
    const tag = parser.previous.tag;

    try parsePrecedence(.prec_unary);

    switch(tag) {
        .token_minus => try emitOp(.op_negate),
        else => unreachable,
    }
}

fn binary() ParseError!void {
    const tag = parser.previous.tag;
    
    const rule = getRule(tag);
    try parsePrecedence(@intToEnum(Precedence, rule.precedence + 1));
    
    switch(tag) {
        .token_plus => try emitOp(.op_add),
        .token_minus => try emitOp(.op_subtract),
        .token_star => try emitOp(.op_multiply),
        .token_slash => try emitOp(.op_divide),
        else => unreachable,
    }
}

fn endCompiler() !void {
    try emitOp(.op_print);
    try emitOp(.op_pop);
    try emitOp(.op_return);

    if (debug.print_code) {
        const writer = std.io.getStdOut().writer();
        if (!parser.hadError) {
            try debug.disassembleChunk(writer, &compiling_chunk, "code");
        }
    }
}

fn makeConstant(value: f64) !u8 {
    const index = try compiling_chunk.addConstant(value);

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
        if (parser.current.tag != .token_error) break;
        
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
    
    if (token.tag == .token_eof) {
        try stderr.print("at end", .{});
    } else if (token.tag == .token_error) {
        // nothing
    } else {
        try stderr.print("at '{}'", .{token.span});
    }
    
    try stderr.print(": {}\n", .{message});
    parser.hadError = true; 
}