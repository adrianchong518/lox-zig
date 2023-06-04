const std = @import("std");
const mem = std.mem;
const isDigit = std.ascii.isDigit;

pub const Token = struct {
    typ: Type,
    lexeme: []const u8,
    line: usize,

    pub const Type = enum(u8) {
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

        @"and",
        class,
        @"continue",
        @"else",
        false,
        @"for",
        fun,
        @"if",
        nil,
        @"or",
        print,
        @"return",
        super,
        this,
        true,
        @"var",
        @"while",
        @"error",
        eof,
    };

    pub fn eqlLexeme(self: Token, other: Token) bool {
        return mem.eql(u8, self.lexeme, other.lexeme);
    }
};

pub const Scanner = struct {
    source: []const u8,
    start: usize,
    current: usize,
    line: usize,

    pub fn init(source: []const u8) Scanner {
        return .{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skip_whitespace();
        self.start = self.current;

        if (self.isAtEnd()) return self.makeToken(.eof);

        const c = self.advance();

        if (isIdentiferAlpha(c)) return self.identifierToken();
        if (isDigit(c)) return self.numberToken();

        switch (c) {
            '(' => return self.makeToken(.left_paren),
            ')' => return self.makeToken(.right_paren),
            '{' => return self.makeToken(.left_brace),
            '}' => return self.makeToken(.right_brace),
            ';' => return self.makeToken(.semicolon),
            ',' => return self.makeToken(.comma),
            '.' => return self.makeToken(.dot),
            '-' => return self.makeToken(.minus),
            '+' => return self.makeToken(.plus),
            '/' => return self.makeToken(.slash),
            '*' => return self.makeToken(.star),

            '!' => return self.makeToken(if (self.match('=')) .bang_equal else .bang),
            '=' => return self.makeToken(if (self.match('=')) .equal_equal else .equal),
            '<' => return self.makeToken(if (self.match('=')) .less_equal else .less),
            '>' => return self.makeToken(if (self.match('=')) .greater_equal else .greater),

            '"' => return self.stringToken(),

            else => return self.errorToken("Unexpected character."),
        }
    }

    fn makeToken(self: Scanner, typ: Token.Type) Token {
        return .{
            .typ = typ,
            .lexeme = self.source[self.start..self.current],
            .line = self.line,
        };
    }

    fn errorToken(self: Scanner, message: []const u8) Token {
        return .{
            .typ = .@"error",
            .lexeme = message,
            .line = self.line,
        };
    }

    fn stringToken(self: *Scanner) Token {
        while (!self.isAtEnd() and self.peek() != '"') {
            if (self.peek() == '\n') self.line += 1;
            self.advanceDrop();
        }

        if (self.isAtEnd()) return self.errorToken("Unterminated string.");

        // Closing quote
        self.advanceDrop();
        return self.makeToken(.string);
    }

    fn numberToken(self: *Scanner) Token {
        while (isDigit(self.peek())) self.advanceDrop();

        // Look for if there is a fractional part
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // Consume the '.'
            self.advanceDrop();
            while (isDigit(self.peek())) self.advanceDrop();
        }

        return self.makeToken(.number);
    }

    fn identifierToken(self: *Scanner) Token {
        while (isIdentiferAlpha(self.peek()) or isDigit(self.peek())) self.advanceDrop();
        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Scanner) Token.Type {
        switch (self.source[self.start]) {
            'a' => return self.checkKeyword(1, "nd", .@"and"),
            'e' => return self.checkKeyword(1, "lse", .@"else"),
            'i' => return self.checkKeyword(1, "f", .@"if"),
            'n' => return self.checkKeyword(1, "il", .nil),
            'o' => return self.checkKeyword(1, "r", .@"or"),
            'p' => return self.checkKeyword(1, "rint", .print),
            'r' => return self.checkKeyword(1, "eturn", .@"return"),
            's' => return self.checkKeyword(1, "uper", .super),
            'v' => return self.checkKeyword(1, "ar", .@"var"),
            'w' => return self.checkKeyword(1, "hile", .@"while"),

            'c' => {
                if (self.tokenLength() > 1) {
                    switch (self.source[self.start + 1]) {
                        'l' => return self.checkKeyword(2, "ass", .class),
                        'o' => return self.checkKeyword(2, "ntinue", .@"continue"),
                        else => {},
                    }
                }
            },

            'f' => {
                if (self.tokenLength() > 1) {
                    switch (self.source[self.start + 1]) {
                        'a' => return self.checkKeyword(2, "lse", .false),
                        'o' => return self.checkKeyword(2, "r", .@"for"),
                        'u' => return self.checkKeyword(2, "n", .fun),
                        else => {},
                    }
                }
            },

            't' => {
                if (self.tokenLength() > 1) {
                    switch (self.source[self.start + 1]) {
                        'h' => return self.checkKeyword(2, "is", .this),
                        'r' => return self.checkKeyword(2, "ue", .true),
                        else => {},
                    }
                }
            },

            else => {},
        }

        return .identifier;
    }

    fn checkKeyword(
        self: Scanner,
        start_offset: usize,
        rest: []const u8,
        typ: Token.Type,
    ) Token.Type {
        const start = self.start + start_offset;
        if (self.tokenLength() == start_offset + rest.len and
            mem.eql(u8, self.source[start..(start + rest.len)], rest))
        {
            return typ;
        }
        return .identifier;
    }

    fn isAtEnd(self: Scanner) bool {
        return self.current >= self.source.len;
    }

    fn tokenLength(self: Scanner) usize {
        return self.current - self.start;
    }

    fn peek(self: Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    fn peekNext(self: Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current + 1];
    }

    fn advance(self: *Scanner) u8 {
        const char = self.peek();
        self.current += 1;
        return char;
    }

    fn advanceDrop(self: *Scanner) void {
        self.current += 1;
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.peek() != expected) return false;
        self.current += 1;
        return true;
    }

    fn skip_whitespace(self: *Scanner) void {
        while (!self.isAtEnd()) {
            switch (self.peek()) {
                ' ', '\r', '\t' => self.advanceDrop(),

                '\n' => {
                    self.line += 1;
                    self.advanceDrop();
                },

                '/' => {
                    if (self.peekNext() == '/') {
                        // A comment goes until the end of the line or EOF
                        while (self.peek() != '\n' and !self.isAtEnd()) self.advanceDrop();
                    } else {
                        return;
                    }
                },

                else => break,
            }
        }
    }
};

fn isIdentiferAlpha(c: u8) bool {
    return switch (c) {
        'a'...'z', 'A'...'Z', '_' => true,
        else => false,
    };
}
