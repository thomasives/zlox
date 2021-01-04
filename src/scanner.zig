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
    token_left_paren,
    token_right_paren,
    token_left_brace,
    token_right_brace,
    token_comma,
    token_dot,
    token_minus,
    token_plus,
    token_semicolon,
    token_slash,
    token_star,

    token_bang,
    token_bang_equal,
    token_equal,
    token_equal_equal,
    token_greater,
    token_greater_equal,
    token_less,
    token_less_equal,

    token_identifier,
    token_string,
    token_number,

    token_and,
    token_class,
    token_else,
    token_false,
    token_for,
    token_fun,
    token_if,
    token_nil,
    token_or,
    token_print,
    token_return,
    token_super,
    token_this,
    token_true,
    token_var,
    token_while,

    token_error,
    token_eof
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
    
    if (isAtEnd()) return makeToken(.token_eof);
    
    var c = advance();
    if (isAlpha(c)) return identifier();
    if (isDigit(c)) return number();
    
    switch(c) {
        '(' => return makeToken(.token_left_paren),
        ')' => return makeToken(.token_right_paren),
        '{' => return makeToken(.token_left_brace),
        '}' => return makeToken(.token_right_brace),
        ';' => return makeToken(.token_semicolon),
        ',' => return makeToken(.token_comma),
        '.' => return makeToken(.token_dot),
        '-' => return makeToken(.token_minus),
        '+' => return makeToken(.token_plus),
        '/' => return makeToken(.token_slash),
        '*' => return makeToken(.token_star),
        '!' => return makeToken(if (match('=')) .token_bang_equal else .token_bang),
        '>' => return makeToken(if (match('=')) .token_greater_equal else .token_greater),
        '<' => return makeToken(if (match('=')) .token_less_equal else .token_less),
        '=' => return makeToken(if (match('=')) .token_equal_equal else .token_equal),
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
        'a' => return checkKeyword(ident[1..], "nd", .token_and),
        'c' => return checkKeyword(ident[1..], "lass", .token_class),
        'e' => return checkKeyword(ident[1..], "lse", .token_else),
        'f' => {
            if (ident.len > 1) {
                switch (ident[1]) {
                    'a' => return checkKeyword(ident[2..], "lse", .token_false),
                    'o' => return checkKeyword(ident[2..], "r", .token_for),
                    'u' => return checkKeyword(ident[2..], "n", .token_fun),
                    else => {}
                }
            }
        },
        'i' => return checkKeyword(ident[1..], "f", .token_if),
        'n' => return checkKeyword(ident[1..], "il", .token_nil),
        'o' => return checkKeyword(ident[1..], "r", .token_or),
        'p' => return checkKeyword(ident[1..], "rint", .token_print),
        'r' => return checkKeyword(ident[1..], "eturn", .token_return),
        's' => return checkKeyword(ident[1..], "uper", .token_super),
        't' => {
            if (ident.len > 1) {
                switch(ident[1]) {
                    'h' => return checkKeyword(ident[2..], "is", .token_this),
                    'r' => return checkKeyword(ident[2..], "ue", .token_true),
                    else => {}
                }
            }
        },
        'v' => return checkKeyword(ident[1..], "ar", .token_var),
        'w' => return checkKeyword(ident[1..], "hile", .token_while),
        else => {}
    }
    
    return .token_identifier;
}

fn checkKeyword(rest: []const u8, target: []const u8, tag: TokenTag) TokenTag {
    if (std.mem.eql(u8, rest, target)) {
        return tag;
    } else {
        return .token_identifier;
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
    
    return makeToken(.token_number);
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
    
    return makeToken(.token_string);
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
    return Token {
        .tag = tag,
        .span = scanner.source_code[scanner.start..scanner.current],
        .line = scanner.line,
    };
}

fn errorToken(message: []const u8) Token {
    return Token {
        .tag = .token_error,
        .span = message,
        .line = scanner.line,
    };
}

fn skipWhitespace() void {
    while(true) {
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
            else => return
        }
    }
}

fn isDigit(c: ?u8) bool {
    return c != null and c.? >= '0' and c.? <= '9';
}

fn isAlpha(c: ?u8) bool {
    return c != null and (
        (c.? >= 'a' and c.? <= 'z') or 
        (c.? >= 'A' and c.? <= 'Z') or 
        c.? == '_');
}