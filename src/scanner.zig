const std = @import("std");

const Scanner = struct {
    const Self = @This();

    source_code: []const u8,
    start: usize,
    current: usize,

    line: usize,
};

var scanner: Scanner = undefined;

pub const TokenTag = enum {
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,
    identifier,
    string,
    number,
    and_,
    class,
    else_,
    false_,
    for_,
    fun,
    if_,
    nil,
    or_,
    print,
    return_,
    super,
    this,
    true_,
    var_,
    while_,
    error_,
    eof,
};

pub const Token = struct {
    tag: TokenTag,
    span: []const u8,
    line: usize,
};

pub fn init(source_code: []const u8) void {
    scanner.source_code = source_code;
    scanner.start = 0;
    scanner.current = 0;
    scanner.line = 1;
}

pub fn nextToken() Token {
    skipWhitespace();

    scanner.start = scanner.current;

    if (isAtEnd()) return makeToken(.eof);

    var c = advance();
    if (isAlpha(c)) return identifier();
    if (isDigit(c)) return number();

    switch (c) {
        '(' => return makeToken(.left_paren),
        ')' => return makeToken(.right_paren),
        '{' => return makeToken(.left_brace),
        '}' => return makeToken(.right_brace),
        ';' => return makeToken(.semicolon),
        ',' => return makeToken(.comma),
        '.' => return makeToken(.dot),
        '-' => return makeToken(.minus),
        '+' => return makeToken(.plus),
        '/' => return makeToken(.slash),
        '*' => return makeToken(.star),
        '!' => return makeToken(if (match('=')) .bang_equal else .bang),
        '>' => return makeToken(if (match('=')) .greater_equal else .greater),
        '<' => return makeToken(if (match('=')) .less_equal else .less),
        '=' => return makeToken(if (match('=')) .equal_equal else .equal),
        '"' => return string(),
        else => {},
    }

    return errorToken("Unexpected character.");
}

fn identifier() Token {
    while (isAlpha(peek()) or isDigit(peek())) _ = advance();

    return makeToken(identifierTag());
}

fn identifierTag() TokenTag {
    const ident = scanner.source_code[scanner.start..scanner.current];
    switch (ident[0]) {
        'a' => return checkKeyword(ident[1..], "nd", .and_),
        'c' => return checkKeyword(ident[1..], "lass", .class),
        'e' => return checkKeyword(ident[1..], "lse", .else_),
        'f' => {
            if (ident.len > 1) {
                switch (ident[1]) {
                    'a' => return checkKeyword(ident[2..], "lse", .false_),
                    'o' => return checkKeyword(ident[2..], "r", .for_),
                    'u' => return checkKeyword(ident[2..], "n", .fun),
                    else => {},
                }
            }
        },
        'i' => return checkKeyword(ident[1..], "f", .if_),
        'n' => return checkKeyword(ident[1..], "il", .nil),
        'o' => return checkKeyword(ident[1..], "r", .or_),
        'p' => return checkKeyword(ident[1..], "rint", .print),
        'r' => return checkKeyword(ident[1..], "eturn", .return_),
        's' => return checkKeyword(ident[1..], "uper", .super),
        't' => {
            if (ident.len > 1) {
                switch (ident[1]) {
                    'h' => return checkKeyword(ident[2..], "is", .this),
                    'r' => return checkKeyword(ident[2..], "ue", .true_),
                    else => {},
                }
            }
        },
        'v' => return checkKeyword(ident[1..], "ar", .var_),
        'w' => return checkKeyword(ident[1..], "hile", .while_),
        else => {},
    }

    return .identifier;
}

fn checkKeyword(rest: []const u8, target: []const u8, tag: TokenTag) TokenTag {
    if (std.mem.eql(u8, rest, target)) {
        return tag;
    } else {
        return .identifier;
    }
}

fn number() Token {
    while (isDigit(peek())) _ = advance();

    const next = peek();
    if (next != null and next.? == '.' and isDigit(peekNext())) {
        // Consume the '.'
        _ = advance();

        while (isDigit(peek())) _ = advance();
    }

    return makeToken(.number);
}

fn string() Token {
    var next = peek();
    while (next != null and next.? != '"') : (next = peek()) {
        if (next.? == '\n') scanner.line += 1;
        _ = advance();
    }

    if (next == null) return errorToken("Undetermined string literal.");

    // Consume closing quote
    _ = advance();

    return makeToken(.string);
}

fn advance() u8 {
    const result = scanner.source_code[scanner.current];
    scanner.current += 1;
    return result;
}

fn match(expected: u8) bool {
    if (isAtEnd()) return false;
    if (scanner.source_code[scanner.current] != expected) return false;

    scanner.current += 1;
    return true;
}

fn peek() ?u8 {
    if (isAtEnd()) return null;

    return scanner.source_code[scanner.current];
}

fn peekNext() ?u8 {
    if (scanner.current + 1 >= scanner.source_code.len) return null;

    return scanner.source_code[scanner.current + 1];
}

fn isAtEnd() bool {
    return scanner.current >= scanner.source_code.len;
}

fn makeToken(tag: TokenTag) Token {
    return Token{
        .tag = tag,
        .span = scanner.source_code[scanner.start..scanner.current],
        .line = scanner.line,
    };
}

fn errorToken(message: []const u8) Token {
    return Token{
        .tag = .error_,
        .span = message,
        .line = scanner.line,
    };
}

fn skipWhitespace() void {
    while (true) {
        var c = peek();
        if (c == null) return;

        switch (c.?) {
            ' ', '\r', '\t' => _ = advance(),
            '\n' => {
                scanner.line += 1;
                _ = advance();
            },
            '/' => {
                if (peekNext() != null and peekNext().? == '/') {
                    while (peek() != null and peek().? != '\n') _ = advance();
                } else {
                    return;
                }
            },
            else => return,
        }
    }
}

fn isDigit(c: ?u8) bool {
    return c != null and c.? >= '0' and c.? <= '9';
}

fn isAlpha(c: ?u8) bool {
    return c != null and ((c.? >= 'a' and c.? <= 'z') or
        (c.? >= 'A' and c.? <= 'Z') or
        c.? == '_');
}
