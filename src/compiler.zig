const std = @import("std");
const fmt = std.fmt;
const io = std.io;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const EnumArray = std.EnumArray;
const File = std.fs.File;

const config = @import("config");

const debug = @import("debug.zig");
const InterpretError = @import("main.zig").InterpretError;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;
const Object = @import("Object.zig");
const Vm = @import("vm.zig").Vm;

pub fn compile(source: []const u8, vm: *Vm, chunk: *Chunk) InterpretError!void {
    var scanner = Scanner.init(source);

    var compiler = Compiler.init(vm.allocator);
    defer compiler.deinit();

    var parser = Parser.init(vm, &scanner, &compiler, chunk);
    defer parser.deinit();

    try parser.advance();
    while (!try parser.match(.eof)) {
        try parser.declaration();
    }
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

const ParseFn = *const fn (*Parser, bool) Parser.Error!void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,

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

        r.set(.bang, .{ .prefix = Parser.unary, .infix = null, .precedence = .none });
        r.set(.bang_equal, .{ .prefix = null, .infix = Parser.binary, .precedence = .equality });
        r.set(.equal, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.equal_equal, .{ .prefix = null, .infix = Parser.binary, .precedence = .equality });
        r.set(.greater, .{ .prefix = null, .infix = Parser.binary, .precedence = .comparison });
        r.set(.greater_equal, .{ .prefix = null, .infix = Parser.binary, .precedence = .comparison });
        r.set(.less, .{ .prefix = null, .infix = Parser.binary, .precedence = .comparison });
        r.set(.less_equal, .{ .prefix = null, .infix = Parser.binary, .precedence = .comparison });

        r.set(.identifier, .{ .prefix = Parser.variable, .infix = null, .precedence = .none });
        r.set(.string, .{ .prefix = Parser.string, .infix = null, .precedence = .none });
        r.set(.number, .{ .prefix = Parser.number, .infix = null, .precedence = .none });

        r.set(.@"and", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.class, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.@"else", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.false, .{ .prefix = Parser.literal, .infix = null, .precedence = .none });
        r.set(.@"for", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.fun, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.@"if", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.nil, .{ .prefix = Parser.literal, .infix = null, .precedence = .none });
        r.set(.@"or", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.print, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.@"return", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.super, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.this, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.true, .{ .prefix = Parser.literal, .infix = null, .precedence = .none });
        r.set(.@"var", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.@"while", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.@"error", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.eof, .{ .prefix = null, .infix = null, .precedence = .none });

        break :rules r;
    };

    fn get(typ: Token.Type) ParseRule {
        return rules.get(typ);
    }
};

const Compiler = struct {
    locals: ArrayList(Local),
    scope_depth: usize = 0,

    const Local = struct {
        name: Token,
        depth: usize,
        is_initialized: bool,
    };

    fn init(allocator: Allocator) Compiler {
        return .{ .locals = ArrayList(Local).init(allocator) };
    }

    fn deinit(self: *Compiler) void {
        self.locals.deinit();
        self.* = undefined;
    }

    fn addLocal(self: *Compiler, name: Token) Allocator.Error!void {
        try self.locals.append(.{
            .name = name,
            .depth = self.scope_depth,
            .is_initialized = false,
        });
    }

    fn lastLocalPtr(self: *Compiler) *Local {
        return &self.locals.items[self.locals.items.len - 1];
    }
};

const Parser = struct {
    vm: *Vm,

    scanner: *Scanner,
    previous: Token = undefined,
    current: Token = undefined,

    current_compiler: *Compiler,

    compiling_chunk: *Chunk,
    constant_strings: Object.String.HashMap(usize),

    had_error: bool = false,
    panic_mode: bool = false,

    const Error = File.WriteError || Allocator.Error;

    fn init(vm: *Vm, scanner: *Scanner, initial_compiler: *Compiler, compiling_chunk: *Chunk) Parser {
        return .{
            .vm = vm,
            .scanner = scanner,
            .current_compiler = initial_compiler,

            .compiling_chunk = compiling_chunk,
            .constant_strings = Object.String.HashMap(usize).init(vm.allocator),
        };
    }

    fn deinit(self: *Parser) void {
        self.constant_strings.deinit();
        self.* = undefined;
    }

    fn end(self: *Parser) Error!void {
        try self.emitOpCode(.@"return");

        if (config.print_code and !self.had_error) {
            debug.disassemble(self.currentChunk().*, "code");
        }
    }

    fn declaration(self: *Parser) Error!void {
        if (try self.match(.@"var")) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.panic_mode) try self.synchronize();
    }

    fn varDeclaration(self: *Parser) Error!void {
        const global = try self.parseVariable("Expect variable name.");

        if (try self.match(.equal)) {
            try self.expression();
        } else {
            try self.emitOpCode(.nil);
        }

        try self.consume(.semicolon, "Expect ';' after variable declaration.");

        try self.defineVariable(global);
    }

    fn parseVariable(self: *Parser, error_message: []const u8) Error!?Token {
        try self.consume(.identifier, error_message);

        try self.declareVariable(self.previous);
        if (self.inScope()) {
            return null;
        }

        return self.previous;
    }

    fn declareVariable(self: *Parser, name: Token) Error!void {
        if (!self.inScope()) return;

        var i = self.current_compiler.locals.items.len;
        while (i > 0) : (i -= 1) {
            const local = self.current_compiler.locals.items[i - 1];
            if (local.is_initialized and local.depth < self.current_compiler.scope_depth) {
                break;
            }

            if (local.name.eqlLexeme(name)) {
                try self.errorAtPrev("Already a variable with this name in this scope");
            }
        }

        try self.current_compiler.addLocal(name);
    }

    fn defineVariable(self: *Parser, global: ?Token) Error!void {
        if (self.inScope()) {
            self.current_compiler.lastLocalPtr().is_initialized = true;
            return;
        }

        const offset = try self.stringConstant(global.?.lexeme);
        try self.emitOpCode(.{ .define_global = .{ .offset = offset } });
    }

    fn resolveLocal(self: *Parser, name: Token) Error!?usize {
        const compiler = self.current_compiler;
        var i = compiler.locals.items.len;
        while (i > 0) : (i -= 1) {
            const local = compiler.locals.items[i - 1];
            if (local.name.eqlLexeme(name)) {
                if (!local.is_initialized) {
                    try self.errorAtPrev("Can't read local variable in its own initializer.");
                }
                return i - 1;
            }
        }

        return null;
    }

    fn statement(self: *Parser) Error!void {
        if (try self.match(.print)) {
            try self.printStatement();
        } else if (try self.match(.left_brace)) {
            self.beginScope();
            try self.block();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }

    fn printStatement(self: *Parser) Error!void {
        try self.expression();
        try self.consume(.semicolon, "Expect ';' after value.");
        try self.emitOpCode(.print);
    }

    fn expressionStatement(self: *Parser) Error!void {
        try self.expression();
        try self.consume(.semicolon, "Expect ';' after expression.");
        try self.emitOpCode(.pop);
    }

    fn expression(self: *Parser) Error!void {
        try self.parsePrecedence(.assignment);
    }

    fn block(self: *Parser) Error!void {
        while (!self.check(.right_brace) and !self.check(.eof)) {
            try self.declaration();
        }

        try self.consume(.right_brace, "Expect '}' after block.");
    }

    fn beginScope(self: *Parser) void {
        self.current_compiler.scope_depth += 1;
    }

    fn endScope(self: *Parser) Allocator.Error!void {
        std.debug.assert(self.current_compiler.scope_depth > 0);
        self.current_compiler.scope_depth -= 1;

        while (self.current_compiler.locals.getLastOrNull()) |local| {
            if (local.depth <= self.current_compiler.scope_depth) break;

            try self.emitOpCode(.pop);
            _ = self.current_compiler.locals.pop();
        }
    }

    fn inScope(self: Parser) bool {
        return self.current_compiler.scope_depth > 0;
    }

    fn number(self: *Parser, _: bool) Error!void {
        const value = fmt.parseFloat(f64, self.previous.lexeme) catch unreachable;
        try self.emitConstant(value);
    }

    fn string(self: *Parser, _: bool) Error!void {
        const bytes = self.previous.lexeme[1 .. self.previous.lexeme.len - 1];
        try self.emitStringConstant(bytes);
    }

    fn variable(self: *Parser, can_assign: bool) Error!void {
        const name = self.previous;
        if (try self.resolveLocal(name)) |offset| {
            try self.emitVariable(
                .{ .get_local = .{ .offset = offset } },
                .{ .set_local = .{ .offset = offset } },
                can_assign,
            );
        } else {
            const offset = try self.stringConstant(name.lexeme);
            try self.emitVariable(
                .{ .get_global = .{ .offset = offset } },
                .{ .set_global = .{ .offset = offset } },
                can_assign,
            );
        }
    }

    fn emitVariable(self: *Parser, get_op: OpCode, set_op: OpCode, can_assign: bool) Error!void {
        if (can_assign and try self.match(.equal)) {
            try self.expression();
            try self.emitOpCode(set_op);
        } else {
            try self.emitOpCode(get_op);
        }
    }

    fn unary(self: *Parser, _: bool) Error!void {
        const op_type = self.previous.typ;

        // Compile the operand
        try self.parsePrecedence(.unary);

        // Emit the operator instruction
        switch (op_type) {
            .bang => try self.emitOpCode(.not),
            .minus => try self.emitOpCode(.negate),
            else => unreachable,
        }
    }

    fn binary(self: *Parser, _: bool) Error!void {
        const tok_type = self.previous.typ;
        const rule = ParseRule.get(tok_type);
        try self.parsePrecedence(rule.precedence.next());

        switch (tok_type) {
            .plus => try self.emitOpCode(.add),
            .minus => try self.emitOpCode(.subtract),
            .star => try self.emitOpCode(.multiply),
            .slash => try self.emitOpCode(.divide),

            .equal_equal => try self.emitOpCode(.equal),
            .greater => try self.emitOpCode(.greater),
            .less => try self.emitOpCode(.less),
            .bang_equal => {
                try self.emitOpCode(.equal);
                try self.emitOpCode(.not);
            },
            .greater_equal => {
                try self.emitOpCode(.less);
                try self.emitOpCode(.not);
            },
            .less_equal => {
                try self.emitOpCode(.greater);
                try self.emitOpCode(.not);
            },

            else => unreachable,
        }
    }

    fn literal(self: *Parser, _: bool) Error!void {
        switch (self.previous.typ) {
            .nil => try self.emitOpCode(.nil),
            .true => try self.emitOpCode(.true),
            .false => try self.emitOpCode(.false),
            else => unreachable,
        }
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) Error!void {
        try self.advance();
        const prefix_rule = ParseRule.get(self.previous.typ).prefix;

        const can_assign = precedence.compare(.lte, .assignment);
        if (prefix_rule) |rule| {
            try rule(self, can_assign);
        } else {
            try self.errorAtPrev("Expect expression");
        }

        while (precedence.compare(.lte, ParseRule.get(self.current.typ).precedence)) {
            try self.advance();
            const infix_rule = ParseRule.get(self.previous.typ).infix;
            if (infix_rule) |rule| try rule(self, can_assign);
        }

        if (can_assign and try self.match(.equal)) {
            try self.errorAtPrev("Invalid assignment target.");
        }
    }

    fn grouping(self: *Parser, _: bool) Error!void {
        try self.expression();
        try self.consume(.right_paren, "Expect ')' after expression");
    }

    fn advance(self: *Parser) Error!void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (!self.check(.@"error")) break;
            try self.errorAtCurrent(self.current.lexeme);
        }
    }

    fn consume(self: *Parser, typ: Token.Type, message: []const u8) Error!void {
        if (self.check(typ)) {
            try self.advance();
            return;
        }

        try self.errorAtCurrent(message);
    }

    fn match(self: *Parser, typ: Token.Type) Error!bool {
        if (!self.check(typ)) return false;
        try self.advance();
        return true;
    }

    fn check(self: *Parser, typ: Token.Type) bool {
        return self.current.typ == typ;
    }

    fn currentChunk(self: *Parser) *Chunk {
        return self.compiling_chunk;
    }

    fn synchronize(self: *Parser) Error!void {
        self.panic_mode = false;

        while (!self.check(.eof)) {
            if (self.previous.typ == .semicolon) return;
            switch (self.current.typ) {
                .class, .fun, .@"var", .@"for", .@"if", .@"while", .print, .@"return" => return,
                else => try self.advance(),
            }
        }
    }

    fn emit(self: *Parser, byte: u8) Allocator.Error!void {
        try self.currentChunk().write(byte, self.previous.line);
    }

    fn emitOpCode(self: *Parser, op_code: OpCode) Allocator.Error!void {
        try self.currentChunk().writeOpCode(op_code, self.previous.line);
    }

    fn emitConstant(self: *Parser, value: anytype) Allocator.Error!void {
        const chunk = self.currentChunk();
        const offset = try chunk.addConstant(value);
        try chunk.writeOpCode(.{ .constant = .{ .offset = offset } }, self.previous.line);
    }

    fn emitStringConstant(self: *Parser, bytes: []const u8) Allocator.Error!void {
        const offset = try self.stringConstant(bytes);
        try self
            .currentChunk()
            .writeOpCode(.{ .constant = .{ .offset = offset } }, self.previous.line);
    }

    fn stringConstant(self: *Parser, bytes: []const u8) Allocator.Error!usize {
        if (self.constant_strings.getAdapted(bytes, Object.String.SliceContext{})) |offset| {
            return offset;
        }

        const identifier = try Object.String.createCopy(self.vm, bytes);
        const offset = try self.currentChunk().addConstant(identifier);
        try self.constant_strings.put(identifier, offset);
        return offset;
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) File.WriteError!void {
        try self.errorAt(self.current, message);
    }

    fn errorAtPrev(self: *Parser, message: []const u8) File.WriteError!void {
        try self.errorAt(self.previous, message);
    }

    fn errorAt(self: *Parser, token: Token, message: []const u8) File.WriteError!void {
        if (self.panic_mode) return;
        self.panic_mode = true;

        const stderr = io.getStdErr().writer();

        try stderr.print("[line {}] Error", .{token.line});

        switch (token.typ) {
            .@"error" => {},
            .eof => try stderr.writeAll(" at end"),
            else => try stderr.print(" at '{s}'", .{token.lexeme}),
        }

        try stderr.print(": {s}\n", .{message});
        self.had_error = true;
    }
};
