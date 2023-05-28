const std = @import("std");
const fmt = std.fmt;
const io = std.io;
const Allocator = std.mem.Allocator;
const EnumArray = std.EnumArray;
const File = std.fs.File;

const config = @import("config");

const debug = @import("debug.zig");
const InterpretError = @import("root").InterpretError;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;

pub fn compile(source: []const u8, chunk: *Chunk) InterpretError!void {
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner, chunk);

    _ = ParseRule.get(.number);

    try parser.advance();
    try parser.expression();
    try parser.consume(.eof, "Expect end of expression");
    try parser.end();

    if (parser.had_error) return error.CompileFailed;
}

const Precedence = enum {
    none,
    assignment,
    @"or",
    @"and",
    equality,
    comparison,
    term,
    factor,
    unary,
    call,
    primary,

    fn next(self: Precedence) Precedence {
        return switch (self) {
            .none => .assignment,
            .assignment => .@"or",
            .@"or" => .@"and",
            .@"and" => .equality,
            .equality => .comparison,
            .comparison => .term,
            .term => .factor,
            .factor => .unary,
            .unary => .call,
            .call => .primary,
            .primary => unreachable,
        };
    }

    fn compare(self: Precedence, op: std.math.CompareOperator, other: Precedence) bool {
        return std.math.compare(@enumToInt(self), op, @enumToInt(other));
    }
};

const ParseFn = *const fn (*Parser) InterpretError!void;

const ParseRule = struct {
    const rules = rules: {
        var r = EnumArray(Token.Type, ParseRule).initUndefined();

        r.set(.left_paren, .{ .prefix = Parser.grouping, .infix = null, .precedence = .none });
        r.set(.right_paren, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.left_brace, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.right_brace, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.comma, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.dot, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.minus, .{ .prefix = Parser.unary, .infix = Parser.binary, .precedence = .term });
        r.set(.plus, .{ .prefix = null, .infix = Parser.binary, .precedence = .term });
        r.set(.semicolon, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.slash, .{ .prefix = null, .infix = Parser.binary, .precedence = .factor });
        r.set(.star, .{ .prefix = null, .infix = Parser.binary, .precedence = .factor });

        r.set(.bang, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.bang_equal, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.equal, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.equal_equal, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.greater, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.greater_equal, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.less, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.less_equal, .{ .prefix = null, .infix = null, .precedence = .none });

        r.set(.identifier, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.string, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.number, .{ .prefix = Parser.number, .infix = null, .precedence = .none });

        r.set(.@"and", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.class, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.@"else", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.false, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.@"for", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.fun, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.@"if", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.nil, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.@"or", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.print, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.@"return", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.super, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.this, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.true, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.@"var", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.@"while", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.@"error", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.eof, .{ .prefix = null, .infix = null, .precedence = .none });

        break :rules r;
    };

    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,

    fn get(typ: Token.Type) ParseRule {
        return rules.get(typ);
    }
};

const Parser = struct {
    scanner: *Scanner,
    previous: Token,
    current: Token,

    compiling_chunk: *Chunk,

    had_error: bool,
    panic_mode: bool,

    fn init(scanner: *Scanner, compiling_chunk: *Chunk) Parser {
        return .{
            .scanner = scanner,
            .previous = undefined,
            .current = undefined,

            .compiling_chunk = compiling_chunk,

            .had_error = false,
            .panic_mode = false,
        };
    }

    fn end(self: *Parser) InterpretError!void {
        try self.emitOpCode(.@"return");

        if (config.print_code and !self.had_error) {
            debug.disassemble(self.currentChunk(), "code");
        }
    }

    fn expression(self: *Parser) InterpretError!void {
        try self.parsePrecedence(.assignment);
    }

    fn number(self: *Parser) InterpretError!void {
        const value = fmt.parseFloat(f64, self.previous.lexeme) catch unreachable;
        try self.emitConstant(value);
    }

    fn unary(self: *Parser) InterpretError!void {
        const op_type = self.previous.typ;

        // Compile the operand
        try self.parsePrecedence(.unary);

        // Emit the operator instruction
        switch (op_type) {
            .minus => try self.emitOpCode(.negate),
            else => unreachable,
        }
    }

    fn binary(self: *Parser) InterpretError!void {
        const tok_type = self.previous.typ;
        const rule = ParseRule.get(tok_type);
        try self.parsePrecedence(rule.precedence.next());

        switch (tok_type) {
            .plus => try self.emitOpCode(.add),
            .minus => try self.emitOpCode(.subtract),
            .star => try self.emitOpCode(.multiply),
            .slash => try self.emitOpCode(.divide),
            else => unreachable,
        }
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) InterpretError!void {
        try self.advance();
        const prefix_rule = ParseRule.get(self.previous.typ).prefix;

        if (prefix_rule) |rule| {
            try rule(self);
        } else {
            try self.errorAtPrev("Expect expression");
        }

        while (precedence.compare(.lte, ParseRule.get(self.current.typ).precedence)) {
            try self.advance();
            const infix_rule = ParseRule.get(self.previous.typ).infix;
            if (infix_rule) |rule| try rule(self);
        }
    }

    fn grouping(self: *Parser) InterpretError!void {
        try self.expression();
        try self.consume(.right_paren, "Expect ')' after expression");
    }

    fn advance(self: *Parser) InterpretError!void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.typ != .@"error") break;
            try self.errorAtCurrent(self.current.lexeme);
        }
    }

    fn consume(self: *Parser, typ: Token.Type, message: []const u8) InterpretError!void {
        if (self.current.typ == typ) {
            try self.advance();
            return;
        }

        try self.errorAtCurrent(message);
    }

    fn currentChunk(self: *Parser) *Chunk {
        return self.compiling_chunk;
    }

    fn emit(self: *Parser, byte: u8) InterpretError!void {
        try self.currentChunk().write(byte, self.previous.line);
    }

    fn emitOpCode(self: *Parser, op_code: OpCode) InterpretError!void {
        try self.currentChunk().writeOpCode(op_code, self.previous.line);
    }

    fn emitConstant(self: *Parser, value: anytype) InterpretError!void {
        self.currentChunk().writeConstant(value, self.previous.line) catch |err| switch (err) {
            error.TooManyConstants => {
                try self.errorAtPrev(fmt.comptimePrint(
                    "Too many constants in one chunk (max: {})",
                    .{Chunk.constant_max_amount},
                ));
            },
            else => |e| return e,
        };
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) File.WriteError!void {
        try self.errorAt(&self.current, message);
    }

    fn errorAtPrev(self: *Parser, message: []const u8) File.WriteError!void {
        try self.errorAt(&self.previous, message);
    }

    fn errorAt(self: *Parser, token: *const Token, message: []const u8) File.WriteError!void {
        if (self.panic_mode) return;
        self.panic_mode = true;

        const stderr = io.getStdErr().writer();

        try stderr.print("[line {}] Error", .{token.line});

        switch (token.typ) {
            .@"error" => {},
            .eof => try stderr.writeAll(" at end"),
            else => try stderr.print(" at {s}", .{token.lexeme}),
        }

        try stderr.print(": {s}\n", .{message});
        self.had_error = true;
    }
};
