const std = @import("std");

const scanner = @import("scanner.zig");
const vm = @import("vm.zig");
const debug = @import("debug.zig");
const vl = @import("value.zig");

const Allocator = std.mem.Allocator;
const Stack = @import("stack.zig").Stack;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Token = scanner.Token;
const TokenTag = scanner.TokenTag;
const Value = vl.Value;
const Function = vl.Function;

const Parser = struct {
    current: Token = undefined,
    previous: Token = undefined,
    hadError: bool = false,
    panicMode: bool = false,
};

const Compiler = struct {
    const FunctionType = enum {
        script,
        function,
    };
    const Local = struct {
        name: Token,
        depth: i64,
        is_captured: bool,
    };
    const Upvalue = struct {
        index: u8,
        is_local: bool,
    };

    const stack_max = std.math.maxInt(u8) + 1;
    const Locals = Stack(Local, stack_max);
    const Upvalues = Stack(Upvalue, stack_max);

    enclosing: ?*Compiler = null,
    function: *Function,
    ty: FunctionType,

    locals: Locals = .{},
    upvalues: Upvalues = .{},
    scope_depth: i64 = 0,
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
const PrefixFn = fn (bool) ParseError!void;
const InfixFn = fn () ParseError!void;

const ParseError = anyerror;

const ParseRule = struct {
    prefix: ?PrefixFn,
    infix: ?InfixFn,
    precedence: Precedence,
};

const rules_table = blk: {
    var table = [_]ParseRule{.{
        .prefix = null,
        .infix = null,
        .precedence = .none,
    }} ** @typeInfo(TokenTag).Enum.fields.len;

    table[@enumToInt(TokenTag.left_paren)] = ParseRule{
        .prefix = grouping,
        .infix = call,
        .precedence = .call,
    };

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

    table[@enumToInt(TokenTag.identifier)].prefix = variable;

    table[@enumToInt(TokenTag.and_)] = ParseRule{
        .prefix = null,
        .infix = and_,
        .precedence = .and_,
    };

    table[@enumToInt(TokenTag.or_)] = ParseRule{
        .prefix = null,
        .infix = or_,
        .precedence = .or_,
    };

    break :blk table;
};

fn getRule(tag: TokenTag) *const ParseRule {
    return &rules_table[@enumToInt(tag)];
}

var parser = Parser{};
var current: *Compiler = undefined;

pub fn compile(allocator: *Allocator, source_code: []const u8) !*Function {
    scanner.init(source_code);
    var compiler = Compiler{
        .function = try vm.newFunction(),
        .ty = .script,
    };
    current = &compiler;

    parser.hadError = false;
    parser.panicMode = false;

    try advance();
    while (!try match(.eof)) {
        try declaration();
    }

    const func = try endCompiler();

    if (parser.hadError) {
        return vm.InterpretError.Compilation;
    }

    return func;
}

fn parsePrecedence(prec: Precedence) ParseError!void {
    try advance();
    const prefixFn = getRule(parser.previous.tag).prefix;
    if (prefixFn == null) {
        try errorAtPrevious("Expect expression.");
        return;
    }

    const can_assign = @enumToInt(prec) <= @enumToInt(Precedence.assignment);
    try prefixFn.?(can_assign);

    while (@enumToInt(prec) <= @enumToInt(getRule(parser.current.tag).precedence)) {
        try advance();
        const infixFn = getRule(parser.previous.tag).infix;
        try infixFn.?();
    }
}

fn declaration() ParseError!void {
    if (try match(.fun)) {
        try funDeclaration();
    } else if (try match(.var_)) {
        try varDeclaration();
    } else {
        try statement();
    }

    if (parser.panicMode) try synchronize();
}

fn funDeclaration() ParseError!void {
    const global = try parseVariable("Expect function name.");
    markInitialized();
    try function(.function);
    try defineVariable(global);
}

fn function(ty: Compiler.FunctionType) ParseError!void {
    var compiler = Compiler{
        .enclosing = current,
        .function = try vm.newFunction(),
        .ty = ty,
    };
    if (ty != .script) {
        const name = try vm.createString(parser.previous.span);
        compiler.function.name = name.cast(vl.String).?;
    }
    compiler.locals.push(Compiler.Local{
        .depth = 0,
        .is_captured = false,
        .name = scanner.Token{
            .tag = undefined,
            .span = "",
            .line = parser.previous.line,
        },
    });

    current = &compiler;
    try beginScope();

    try consume(.left_paren, "Expect '(' after function name.");
    if (!check(.right_paren)) {
        while (true) {
            current.function.arity += 1;
            if (current.function.arity > 255) {
                try errorAtCurrent("Can't have more than 255 parameters");
            }

            const param = try parseVariable("Expected parameter name.");
            try defineVariable(param);

            if (!try match(.comma)) break;
        }
    }
    try consume(.right_paren, "Expect ')' after function name.");

    try consume(.left_brace, "Expect '{' before function body.");
    try block();

    const func = try endCompiler();
    current = compiler.enclosing.?;
    try emitOpByte(.closure, try makeConstant(Value{ .obj = &func.base }));

    for (compiler.upvalues.slice()) |upvalue| {
        try emitByte(if (upvalue.is_local) 1 else 0);
        try emitByte(upvalue.index);
    }
}

fn varDeclaration() ParseError!void {
    var global = try parseVariable("Expected variable name.");

    if (try match(.equal)) {
        try expression();
    } else {
        try emitOp(.nil);
    }
    try consume(.semicolon, "Expect ';' after variable declaration.");

    try defineVariable(global);
}

fn parseVariable(errorMessage: []const u8) ParseError!u8 {
    try consume(.identifier, errorMessage);

    try declareVariable();
    if (current.scope_depth > 0) return @as(u8, 0);

    return identifierConstant(&parser.previous);
}

fn declareVariable() ParseError!void {
    if (current.scope_depth == 0) return;

    const name: *Token = &parser.previous;
    {
        var i: usize = 0;
        while (i < current.locals.top) : (i += 1) {
            const local = current.locals.peek(i);
            if (local.depth != -1 and local.depth < current.scope_depth) {
                break;
            }

            if (identifiersEqual(name, &local.name)) {
                try errorAtPrevious("Already a variable with this name in this scope.");
            }
        }
    }
    try addLocal(name);
}

fn addLocal(name: *Token) ParseError!void {
    if (current.locals.full()) {
        try errorAtPrevious("Too many local variables in function.");
        return;
    }
    current.locals.push(Compiler.Local{
        .name = name.*,
        .depth = -1,
        .is_captured = false,
    });
}

fn identifiersEqual(a: *Token, b: *Token) bool {
    return std.mem.eql(u8, a.span, b.span);
}

fn defineVariable(global: u8) ParseError!void {
    if (current.scope_depth > 0) {
        markInitialized();
        return;
    }

    try emitOpByte(.define_global, global);
}

fn markInitialized() void {
    if (current.scope_depth == 0) return;
    current.locals.peek(0).depth = current.scope_depth;
}

fn statement() ParseError!void {
    if (try match(.print)) {
        try printStatement();
    } else if (try match(.for_)) {
        try forStatement();
    } else if (try match(.if_)) {
        try ifStatement();
    } else if (try match(.return_)) {
        try returnStatement();
    } else if (try match(.while_)) {
        try whileStatement();
    } else if (try match(.left_brace)) {
        try beginScope();
        try block();
        try endScope();
    } else {
        try expressionStatement();
    }
}

fn block() ParseError!void {
    while (!check(.right_brace) and !check(.eof)) {
        try declaration();
    }

    try consume(.right_brace, "Expect '}' after block.");
}

fn beginScope() ParseError!void {
    current.scope_depth += 1;
}

fn endScope() ParseError!void {
    current.scope_depth -= 1;
    while (!current.locals.empty() and
        current.locals.peek(0).depth > current.scope_depth)
    {
        if (current.locals.peek(0).is_captured) {
            try emitOp(.close_upvalue);
        } else {
            try emitOp(.pop);
        }
        _ = current.locals.pop();
    }
}

fn printStatement() ParseError!void {
    try expression();
    try consume(.semicolon, "Expect ';' after value.");
    try emitOp(.print);
}

fn returnStatement() ParseError!void {
    if (current.ty == .script) {
        try errorAtPrevious("Can't return from top-level code.");
    }

    if (try match(.semicolon)) {
        try emitOp(.nil);
        try emitOp(.return_);
    } else {
        try expression();
        try consume(.semicolon, "Expected ';' after return value.");
        try emitOp(.return_);
    }
}

fn ifStatement() ParseError!void {
    try consume(.left_paren, "Expect '(' after 'if'.");
    try expression();
    try consume(.right_paren, "Expect '(' after 'if'.");

    const thenJump = try emitJump(.jump_if_false);
    try emitOp(.pop);
    try statement();

    const elseJump = try emitJump(.jump);
    try patchJump(thenJump);
    try emitOp(.pop);

    if (try match(.else_)) try statement();
    try patchJump(elseJump);
}

fn whileStatement() ParseError!void {
    const loopStart = current.function.chunk.code.items.len;
    try consume(.left_paren, "Expect '(' after 'while'.");
    try expression();
    try consume(.right_paren, "Expect '(' after 'while'.");

    const exitJump = try emitJump(.jump_if_false);
    try emitOp(.pop);
    try statement();

    try emitLoop(loopStart);

    try patchJump(exitJump);
    try emitOp(.pop);
}

fn forStatement() ParseError!void {
    try beginScope();
    try consume(.left_paren, "Expect '(' after 'for'");

    // Initializer clause
    if (try match(.semicolon)) {
        // No initializer
    } else if (try match(.var_)) {
        try varDeclaration();
    } else {
        try expressionStatement();
    }

    var loopStart = current.function.chunk.code.items.len;

    // Condition clause
    var exitJump: ?usize = null;
    if (!try match(.semicolon)) {
        try expression();
        try consume(.semicolon, "Expect ';' after loop condition.");

        exitJump = try emitJump(.jump_if_false);
        try emitOp(.pop);
    }

    // Increment clause
    if (!try match(.right_paren)) {
        const bodyJump = try emitJump(.jump);

        const incrementStart = current.function.chunk.code.items.len;
        try expression();
        try emitOp(.pop);
        try consume(.right_paren, "Expect ')' after 'for clauses'");

        try emitLoop(loopStart);
        loopStart = incrementStart;
        try patchJump(bodyJump);
    }

    // User code
    try statement();

    try emitLoop(loopStart);

    if (exitJump != null) {
        try patchJump(exitJump.?);
        try emitOp(.pop);
    }

    try endScope();
}

fn expressionStatement() ParseError!void {
    try expression();
    try consume(.semicolon, "Expect ';' after expression.");
    try emitOp(.pop);
}

fn expression() ParseError!void {
    try parsePrecedence(.assignment);
}

fn literal(can_assign: bool) ParseError!void {
    switch (parser.previous.tag) {
        .false_ => try emitOp(.false_),
        .true_ => try emitOp(.true_),
        .nil => try emitOp(.nil),
        else => unreachable,
    }
}

fn number(can_assign: bool) ParseError!void {
    const num = try std.fmt.parseFloat(f64, parser.previous.span);
    const val = Value{ .number = num };
    try emitOpByte(.constant, try makeConstant(val));
}

fn string(can_assign: bool) ParseError!void {
    const allocator = vm.getAllocator();

    const len = parser.previous.span.len;
    var obj = try vm.createString(parser.previous.span[1 .. len - 1]);

    try emitOpByte(.constant, try makeConstant(Value{ .obj = obj }));
}

fn call() ParseError!void {
    const arg_count = try argumentList();
    try emitOpByte(.call, arg_count);
}

fn argumentList() ParseError!u8 {
    var arg_count: u8 = 0;

    if (!check(.right_paren)) {
        while (true) {
            try expression();
            if (arg_count == 255) {
                try errorAtPrevious("Can't have more than 255 arguments.");
            }
            arg_count += 1;
            if (!try match(.comma)) break;
        }
    }

    try consume(.right_paren, "Expect ')' after arguments");
    return arg_count;
}

fn grouping(can_assign: bool) ParseError!void {
    try expression();
    try consume(.right_paren, "Expected ')' after expression.");
}

fn unary(can_assign: bool) ParseError!void {
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

fn and_() ParseError!void {
    const endJump = try emitJump(.jump_if_false);

    try emitOp(.pop);
    try parsePrecedence(.and_);

    try patchJump(endJump);
}

fn or_() ParseError!void {
    const endJump = try emitJump(.jump_if_true);

    try emitOp(.pop);
    try parsePrecedence(.or_);

    try patchJump(endJump);
}

fn variable(can_assign: bool) ParseError!void {
    var get_op: OpCode = undefined;
    var set_op: OpCode = undefined;

    const name = &parser.previous;
    var arg: u8 = undefined;

    if (try resolveLocal(current, name)) |local| {
        arg = local;
        get_op = .get_local;
        set_op = .set_local;
    } else if (try resolveUpvalue(current, name)) |upvalue| {
        arg = upvalue;
        get_op = .get_upvalue;
        set_op = .set_upvalue;
    } else {
        arg = try identifierConstant(&parser.previous);
        get_op = .get_global;
        set_op = .set_global;
    }

    if (can_assign and try match(.equal)) {
        try expression();
        try emitOpByte(set_op, arg);
    } else {
        try emitOpByte(get_op, arg);
    }
}

fn endCompiler() !*Function {
    try emitOp(.nil);
    try emitOp(.return_);
    const func = current.function;
    func.upvalue_count = @intCast(i32, current.upvalues.top);

    if (debug.print_code) {
        const writer = std.io.getStdOut().writer();
        if (!parser.hadError) {
            try debug.disassembleChunk(writer, &current.function.chunk, if (func.name != null) func.name.?.chars else "<script>");
        }
    }

    return func;
}

fn makeConstant(val: Value) !u8 {
    const index = try current.function.chunk.addConstant(val);

    if (index > std.math.maxInt(u8)) {
        try errorAtPrevious("Too many constants for single chunk.");
        return 0;
    }

    return @intCast(u8, index);
}

fn identifierConstant(name: *const scanner.Token) !u8 {
    return try makeConstant(Value{ .obj = try vm.createString(name.span) });
}

fn resolveLocal(compiler: *Compiler, name: *Token) ParseError!?u8 {
    var i: usize = 0;
    while (i < compiler.locals.top) : (i += 1) {
        const local = compiler.locals.peek(i);
        if (identifiersEqual(name, &local.name)) {
            if (local.depth == -1) {
                try errorAtPrevious("Cannot read local variable in its own initializer.");
            }
            return @intCast(u8, compiler.locals.top - i - 1);
        }
    }

    return null;
}

fn resolveUpvalue(compiler: *Compiler, name: *Token) ParseError!?u8 {
    if (compiler.enclosing) |enclosing| {
        if (try resolveLocal(enclosing, name)) |local| {
            enclosing.locals.buffer[local].is_captured = true;
            return try addUpvalue(compiler, local, true);
        } else if (try resolveUpvalue(enclosing, name)) |upvalue| {
            return try addUpvalue(compiler, upvalue, false);
        }
    }
    return null;
}

fn addUpvalue(compiler: *Compiler, index: u8, is_local: bool) ParseError!u8 {
    for (compiler.upvalues.slice()) |upvalue, i| {
        if (upvalue.index == index and upvalue.is_local == is_local) {
            return @intCast(u8, i);
        }
    }

    if (compiler.upvalues.full()) {
        try errorAtPrevious("Too many closure variables in function.");
        return 0;
    }

    compiler.upvalues.push(Compiler.Upvalue{
        .index = index,
        .is_local = is_local,
    });

    return @intCast(u8, compiler.upvalues.top - 1);
}

fn advance() ParseError!void {
    parser.previous = parser.current;

    while (true) {
        parser.current = scanner.nextToken();
        if (parser.current.tag != .error_) break;

        try errorAtCurrent(parser.current.span);
    }
}

fn match(tag: scanner.TokenTag) ParseError!bool {
    if (!check(tag)) return false;
    try advance();
    return true;
}

fn check(tag: scanner.TokenTag) bool {
    return parser.current.tag == tag;
}

fn consume(expected: scanner.TokenTag, error_message: []const u8) !void {
    if (parser.current.tag == expected) {
        try advance();
        return;
    }

    try errorAtCurrent(error_message);
}

fn emitOp(op: OpCode) !void {
    try current.function.chunk.writeOp(op, parser.previous.line);
}

fn emitByte(byte: u8) !void {
    try current.function.chunk.write(byte, parser.previous.line);
}

fn emitOpByte(op: OpCode, byte: u8) !void {
    try current.function.chunk.writeOp(op, parser.previous.line);
    try current.function.chunk.write(byte, parser.previous.line);
}

fn emitJump(jump: OpCode) !usize {
    try emitOp(jump);
    try emitByte(0xff);
    try emitByte(0xff);
    return current.function.chunk.code.items.len - 2;
}

fn emitLoop(start: usize) !void {
    try emitOp(.loop);

    const offset = current.function.chunk.code.items.len - start + 2;
    if (offset > std.math.maxInt(u16)) try errorAtPrevious("Loop body too large.");

    try emitByte(@intCast(u8, (offset >> 8) & 0xff));
    try emitByte(@intCast(u8, offset & 0xff));
}

fn patchJump(offset: usize) !void {
    // -2 to adjust for the bytecode for the jump offset itself.
    const jump = current.function.chunk.code.items.len - offset - 2;

    if (jump > std.math.maxInt(u16)) {
        try errorAtPrevious("Too much code to jump over.");
    }

    current.function.chunk.code.items[offset] = @intCast(u8, (jump >> 8) & 0xff);
    current.function.chunk.code.items[offset + 1] = @intCast(u8, jump & 0xff);
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

fn synchronize() ParseError!void {
    parser.panicMode = false;

    while (parser.current.tag != .eof) {
        if (parser.previous.tag == .semicolon) return;

        switch (parser.current.tag) {
            .class, .fun, .var_, .for_, .if_, .while_, .print, .return_ => return,
            else => {},
        }

        try advance();
    }
}
