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
const Vm = @import("Vm.zig");
const Value = @import("value.zig").Value;

pub fn compile(source: []const u8, vm: *Vm) InterpretError!*Object.Function {
    var scanner = Scanner.init(source);

    var compiler = try Compiler.init(vm, null, .script);
    defer compiler.deinit();

    var parser = Parser.create();
    parser.init(vm, &scanner, &compiler);
    defer parser.deinit();

    try parser.advance();
    while (!try parser.match(.eof)) {
        try parser.declaration();
    }

    const function = try parser.endCompiler();

    if (parser.had_error) return error.CompileFailed;
    return function;
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

        r.set(.left_paren, .{ .prefix = Parser.grouping, .infix = Parser.call, .precedence = .call });
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

        r.set(.@"and", .{ .prefix = null, .infix = Parser.@"and", .precedence = .@"and" });
        r.set(.class, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.@"else", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.false, .{ .prefix = Parser.literal, .infix = null, .precedence = .none });
        r.set(.@"for", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.fun, .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.@"if", .{ .prefix = null, .infix = null, .precedence = .none });
        r.set(.nil, .{ .prefix = Parser.literal, .infix = null, .precedence = .none });
        r.set(.@"or", .{ .prefix = null, .infix = Parser.@"or", .precedence = .@"or" });
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

pub const Compiler = struct {
    enclosing: ?*Compiler,

    function: *Object.Function,
    function_type: FunctionType,
    upvalues: ArrayList(OpCode.Upvalue),

    constant_strings: Object.String.HashMap(usize),

    locals: ArrayList(Local),
    scope_depth: usize = 0,

    const FunctionType = enum { function, script };

    const Local = struct {
        name: Token,
        depth: usize,
        state: State,
        is_captured: bool = false,

        const State = enum { uninitialized, initialized };
    };

    fn init(vm: *Vm, enclosing: ?*Compiler, function_type: FunctionType) Allocator.Error!Compiler {
        var locals = ArrayList(Local).init(vm.allocator);
        try locals.append(.{
            .name = .{ .typ = undefined, .lexeme = "", .line = 0 },
            .depth = 0,
            .state = .initialized,
        });

        return .{
            .enclosing = enclosing,
            .function = try Object.Function.create(vm),
            .function_type = function_type,
            .upvalues = ArrayList(OpCode.Upvalue).init(vm.allocator),
            .constant_strings = Object.String.HashMap(usize).init(vm.allocator),
            .locals = locals,
        };
    }

    fn deinit(self: *Compiler) void {
        self.upvalues.deinit();
        self.constant_strings.deinit();
        self.locals.deinit();
        self.* = undefined;
    }

    fn addLocal(self: *Compiler, name: Token, state: Local.State) Allocator.Error!void {
        try self.locals.append(.{
            .name = name,
            .depth = self.scope_depth,
            .state = state,
        });
    }

    fn addUpvalue(
        self: *Compiler,
        index: usize,
        locality: OpCode.Upvalue.Locality,
    ) Allocator.Error!usize {
        const upvalue = .{
            .index = index,
            .locality = locality,
        };

        for (self.upvalues.items, 0..) |u, i| {
            if (u.eql(upvalue)) return i;
        }

        try self.upvalues.append(upvalue);
        self.function.upvalue_count += 1;
        return self.upvalues.items.len - 1;
    }

    fn lastLocalPtr(self: *Compiler) *Local {
        std.debug.assert(self.locals.items.len > 0);
        return &self.locals.items[self.locals.items.len - 1];
    }

    fn resolveLocal(self: *Compiler, parser: *Parser, name: Token) Parser.Error!?usize {
        var i = self.locals.items.len;
        while (i > 0) : (i -= 1) {
            const local = self.locals.items[i - 1];
            if (local.name.eqlLexeme(name)) {
                if (local.state == .uninitialized) {
                    try parser.errorAtPrev("Can't read local variable in its own initializer.");
                }
                return i - 1;
            }
        }

        return null;
    }

    fn resolveUpvalue(self: *Compiler, parser: *Parser, name: Token) Parser.Error!?usize {
        const enclosing = self.enclosing orelse {
            return null;
        };

        if (try enclosing.resolveLocal(parser, name)) |local| {
            enclosing.locals.items[local].is_captured = true;
            return try self.addUpvalue(local, .local);
        }

        if (try enclosing.resolveUpvalue(parser, name)) |upvalue| {
            return try self.addUpvalue(upvalue, .non_local);
        }

        return null;
    }
};

pub const Parser = struct {
    vm: *Vm,

    scanner: *Scanner,
    previous: Token = undefined,
    current: Token = undefined,

    current_compiler: *Compiler,
    innermostLoop: ?struct {
        start: usize = 0,
        scope_depth: usize = 0,
    } = null,

    had_error: bool = false,
    panic_mode: bool = false,

    const Error = File.WriteError || Allocator.Error;

    fn create() Parser {
        return .{
            .vm = undefined,
            .scanner = undefined,
            .current_compiler = undefined,
        };
    }

    fn init(self: *Parser, vm: *Vm, scanner: *Scanner, initial_compiler: *Compiler) void {
        self.* = .{
            .vm = vm,
            .scanner = scanner,
            .current_compiler = initial_compiler,
        };
        self.vm.gc_allocator.attach_parser(self);
    }

    fn deinit(self: *Parser) void {
        self.vm.gc_allocator.detach_parser();

        self.* = undefined;
    }

    fn startCompiler(self: *Parser, compiler: *Compiler) Allocator.Error!void {
        self.current_compiler = compiler;

        const name = try Object.String.createCopy(self.vm, self.previous.lexeme);
        self.current_compiler.function.name = name;
    }

    fn popCompiler(self: *Parser) Error!*Object.Function {
        std.debug.assert(self.current_compiler.enclosing != null);

        const function = self.endCompiler();
        self.current_compiler = self.current_compiler.enclosing.?;
        return function;
    }

    fn endCompiler(self: *Parser) Error!*Object.Function {
        _ = try self.emitOpCode(.nil);
        _ = try self.emitOpCode(.@"return");
        const function = self.current_compiler.function;

        if (config.print_code and !self.had_error) {
            debug.disassemble(self.currentChunk().*, function);
        }

        return function;
    }

    fn declaration(self: *Parser) Error!void {
        if (try self.match(.fun)) {
            try self.funStatement();
        } else if (try self.match(.@"var")) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.panic_mode) try self.synchronize();
    }

    fn funStatement(self: *Parser) Error!void {
        const global = try self.parseVariable(.initialized, "Expect function name.");
        _ = try self.emitFunction(.function);
        try self.defineVariable(global);
    }

    fn varDeclaration(self: *Parser) Error!void {
        const global = try self.parseVariable(.uninitialized, "Expect variable name.");

        if (try self.match(.equal)) {
            try self.expression();
        } else {
            _ = try self.emitOpCode(.nil);
        }

        try self.consume(.semicolon, "Expect ';' after variable declaration.");

        try self.defineVariable(global);
    }

    fn parseVariable(
        self: *Parser,
        state: Compiler.Local.State,
        error_message: []const u8,
    ) Error!?Token {
        try self.consume(.identifier, error_message);

        try self.declareVariable(self.previous, state);
        if (self.inScope()) {
            return null;
        }

        return self.previous;
    }

    fn declareVariable(self: *Parser, name: Token, state: Compiler.Local.State) Error!void {
        if (!self.inScope()) return;

        var i = self.current_compiler.locals.items.len;
        while (i > 0) : (i -= 1) {
            const local = self.current_compiler.locals.items[i - 1];
            if (local.state == .initialized and local.depth < self.current_compiler.scope_depth) {
                break;
            }

            if (local.name.eqlLexeme(name)) {
                try self.errorAtPrev("Already a variable with this name in this scope");
            }
        }

        try self.current_compiler.addLocal(name, state);
    }

    fn defineVariable(self: *Parser, name: ?Token) Error!void {
        if (self.inScope()) {
            self.current_compiler.lastLocalPtr().state = .initialized;
            return;
        }

        const index = try self.stringConstant(name.?.lexeme);
        _ = try self.emitOpCode(.{ .define_global = .{ .index = index } });
    }

    fn statement(self: *Parser) Error!void {
        if (try self.match(.print)) {
            try self.printStatement();
        } else if (try self.match(.@"if")) {
            try self.ifStatement();
        } else if (try self.match(.@"while")) {
            try self.whileStatement();
        } else if (try self.match(.@"for")) {
            try self.forStatement();
        } else if (try self.match(.@"continue")) {
            try self.continueStatement();
        } else if (try self.match(.@"return")) {
            try self.returnStatement();
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
        _ = try self.emitOpCode(.print);
    }

    fn ifStatement(self: *Parser) Error!void {
        // condition
        try self.consume(.left_paren, "Expect '(' after 'if'.");
        try self.expression();
        try self.consume(.right_paren, "Expect ')' after 'condition'.");

        const then_jump = try self.emitOpCode(.{ .jump_if_false = .{} });

        // then branch
        _ = try self.emitOpCode(.pop);
        try self.statement();
        const else_jump = try self.emitOpCode(.{ .jump = .{} });

        try self.patchJump(then_jump);

        // else branch
        _ = try self.emitOpCode(.pop);
        if (try self.match(.@"else")) try self.statement();

        try self.patchJump(else_jump);
    }

    fn whileStatement(self: *Parser) Error!void {
        const surroundingLoop = self.innermostLoop;
        self.innermostLoop = .{
            .start = self.currentChunk().code.items.len,
            .scope_depth = self.current_compiler.scope_depth,
        };

        // condition
        try self.consume(.left_paren, "Expect '(' after 'if'.");
        try self.expression();
        try self.consume(.right_paren, "Expect ')' after 'condition'.");

        const exit_jump = try self.emitOpCode(.{ .jump_if_false = .{} });

        // body
        _ = try self.emitOpCode(.pop);
        try self.statement();
        _ = try self.emitLoop(self.innermostLoop.?.start);

        try self.patchJump(exit_jump);
        _ = try self.emitOpCode(.pop);

        self.innermostLoop = surroundingLoop;
    }

    fn forStatement(self: *Parser) Error!void {
        self.beginScope();

        try self.consume(.left_paren, "Expect '(' after 'for'.");

        // initializer clause
        if (try self.match(.semicolon)) {
            // No initializer
        } else if (try self.match(.@"var")) {
            try self.varDeclaration();
        } else {
            try self.expressionStatement();
        }

        const surroundingLoop = self.innermostLoop;
        self.innermostLoop = .{
            .start = self.currentChunk().code.items.len,
            .scope_depth = self.current_compiler.scope_depth,
        };

        // loop condition clause
        const exit_jump = if (!try self.match(.semicolon)) cond: {
            try self.expression();
            try self.consume(.semicolon, "Expect ';' after loop condition.");

            const exit_jump = try self.emitOpCode(.{ .jump_if_false = .{} });
            _ = try self.emitOpCode(.pop);

            break :cond exit_jump;
        } else null;

        if (!try self.match(.right_paren)) {
            const body_jump = try self.emitOpCode(.{ .jump = .{} });
            const increment_start = self.currentChunk().code.items.len;
            try self.expression();
            _ = try self.emitOpCode(.pop);
            try self.consume(.right_paren, "Expect ')' after for clauses.");

            _ = try self.emitLoop(self.innermostLoop.?.start);
            self.innermostLoop.?.start = increment_start;

            try self.patchJump(body_jump);
        }

        // body
        try self.statement();
        _ = try self.emitLoop(self.innermostLoop.?.start);

        if (exit_jump) |ej| {
            try self.patchJump(ej);
            _ = try self.emitOpCode(.pop);
        }

        self.innermostLoop = surroundingLoop;
        try self.endScope();
    }

    fn continueStatement(self: *Parser) Error!void {
        try self.consume(.semicolon, "Expect ';' after 'continue'.");

        if (self.innermostLoop) |loop| {
            while (self.current_compiler.locals.getLastOrNull()) |local| {
                if (local.depth <= self.current_compiler.scope_depth) break;
                _ = try self.emitOpCode(.pop);
            }
            _ = try self.emitLoop(loop.start);
        } else {
            try self.errorAtPrev("Can't use 'continue' outside of a loop.");
        }
    }

    fn returnStatement(self: *Parser) Error!void {
        if (self.current_compiler.function_type == .script) {
            try self.errorAtPrev("Can't return from top-level code.");
        }

        if (try self.match(.semicolon)) {
            _ = try self.emitOpCode(.nil);
        } else {
            try self.expression();
            try self.consume(.semicolon, "Expect ';' after return value");
        }

        _ = try self.emitOpCode(.@"return");
    }

    fn expressionStatement(self: *Parser) Error!void {
        try self.expression();
        try self.consume(.semicolon, "Expect ';' after expression.");
        _ = try self.emitOpCode(.pop);
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

            if (local.is_captured) {
                _ = try self.emitOpCode(.close_upvalue);
            } else {
                _ = try self.emitOpCode(.pop);
            }
            _ = self.current_compiler.locals.pop();
        }
    }

    fn inScope(self: Parser) bool {
        return self.current_compiler.scope_depth > 0;
    }

    fn number(self: *Parser, _: bool) Error!void {
        const value = fmt.parseFloat(f64, self.previous.lexeme) catch unreachable;
        _ = try self.emitConstant(value);
    }

    fn string(self: *Parser, _: bool) Error!void {
        const bytes = self.previous.lexeme[1 .. self.previous.lexeme.len - 1];
        _ = try self.emitStringConstant(bytes);
    }

    fn variable(self: *Parser, can_assign: bool) Error!void {
        const name = self.previous;
        if (try self.current_compiler.resolveLocal(self, name)) |index| {
            _ = try self.emitVariable(
                .{ .get_local = .{ .index = index } },
                .{ .set_local = .{ .index = index } },
                can_assign,
            );
        } else if (try self.current_compiler.resolveUpvalue(self, name)) |index| {
            _ = try self.emitVariable(
                .{ .get_upvalue = .{ .index = index } },
                .{ .set_upvalue = .{ .index = index } },
                can_assign,
            );
        } else {
            const index = try self.stringConstant(name.lexeme);
            _ = try self.emitVariable(
                .{ .get_global = .{ .index = index } },
                .{ .set_global = .{ .index = index } },
                can_assign,
            );
        }
    }

    fn @"and"(self: *Parser, _: bool) Error!void {
        const end_jump = try self.emitOpCode(.{ .jump_if_false = .{} });
        _ = try self.emitOpCode(.pop);
        try self.parsePrecedence(.@"and");
        try self.patchJump(end_jump);
    }

    fn @"or"(self: *Parser, _: bool) Error!void {
        const else_jump = try self.emitOpCode(.{ .jump_if_false = .{} });
        const end_jump = try self.emitOpCode(.{ .jump = .{} });
        try self.patchJump(else_jump);

        _ = try self.emitOpCode(.pop);
        try self.parsePrecedence(.@"or");

        try self.patchJump(end_jump);
    }

    fn unary(self: *Parser, _: bool) Error!void {
        const op_type = self.previous.typ;

        // Compile the operand
        try self.parsePrecedence(.unary);

        // Emit the operator instruction
        switch (op_type) {
            .bang => _ = try self.emitOpCode(.not),
            .minus => _ = try self.emitOpCode(.negate),
            else => unreachable,
        }
    }

    fn binary(self: *Parser, _: bool) Error!void {
        const tok_type = self.previous.typ;
        const rule = ParseRule.get(tok_type);
        try self.parsePrecedence(rule.precedence.next());

        switch (tok_type) {
            .plus => _ = try self.emitOpCode(.add),
            .minus => _ = try self.emitOpCode(.subtract),
            .star => _ = try self.emitOpCode(.multiply),
            .slash => _ = try self.emitOpCode(.divide),

            .equal_equal => _ = try self.emitOpCode(.equal),
            .greater => _ = try self.emitOpCode(.greater),
            .less => _ = try self.emitOpCode(.less),
            .bang_equal => {
                _ = try self.emitOpCode(.equal);
                _ = try self.emitOpCode(.not);
            },
            .greater_equal => {
                _ = try self.emitOpCode(.less);
                _ = try self.emitOpCode(.not);
            },
            .less_equal => {
                _ = try self.emitOpCode(.greater);
                _ = try self.emitOpCode(.not);
            },

            else => unreachable,
        }
    }

    fn call(self: *Parser, _: bool) Error!void {
        const arg_count = arg: {
            var arg_count: u8 = 0;
            if (!self.check(.right_paren)) {
                while (true) {
                    try self.expression();

                    if (arg_count >= 255) {
                        try self.errorAtPrev("Can't have more than 255 arguments");
                    } else {
                        arg_count += 1;
                    }

                    if (!try self.match(.comma)) break;
                }
            }
            try self.consume(.right_paren, "Expect ')' after arguments.");
            break :arg arg_count;
        };

        _ = try self.emitOpCode(.{ .call = .{ .arg_count = arg_count } });
    }

    fn literal(self: *Parser, _: bool) Error!void {
        switch (self.previous.typ) {
            .nil => _ = try self.emitOpCode(.nil),
            .true => _ = try self.emitOpCode(.true),
            .false => _ = try self.emitOpCode(.false),
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
        return &self.current_compiler.function.chunk;
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

    fn emitOpCode(self: *Parser, op_code: OpCode) Allocator.Error!usize {
        return self.currentChunk().writeOpCode(op_code, self.previous.line);
    }

    fn emitConstant(self: *Parser, value: anytype) Allocator.Error!usize {
        const chunk = self.currentChunk();
        const index = try chunk.addConstant(value);
        return chunk.writeOpCode(.{ .constant = .{ .index = index } }, self.previous.line);
    }

    fn emitStringConstant(self: *Parser, bytes: []const u8) Allocator.Error!usize {
        const index = try self.stringConstant(bytes);
        return self
            .currentChunk()
            .writeOpCode(.{ .constant = .{ .index = index } }, self.previous.line);
    }

    fn emitVariable(self: *Parser, get_op: OpCode, set_op: OpCode, can_assign: bool) Error!usize {
        if (can_assign and try self.match(.equal)) {
            try self.expression();
            return self.emitOpCode(set_op);
        } else {
            return self.emitOpCode(get_op);
        }
    }

    fn emitFunction(self: *Parser, typ: Compiler.FunctionType) Error!usize {
        var compiler = try Compiler.init(self.vm, self.current_compiler, typ);
        defer compiler.deinit();

        try self.startCompiler(&compiler);
        self.beginScope();

        try self.consume(.left_paren, "Expect '(' after function name.");
        if (!self.check(.right_paren)) {
            while (true) {
                if (self.current_compiler.function.arity >= 255) {
                    try self.errorAtCurrent("Can't have more than 255 parameters.");
                } else {
                    self.current_compiler.function.arity += 1;
                }

                const constant = try self.parseVariable(.initialized, "Expect parameter name.");
                try self.defineVariable(constant);

                if (!try self.match(.comma)) break;
            }
        }
        try self.consume(.right_paren, "Expect ')' after parameters.");

        try self.consume(.left_brace, "Expect '{' before function body.");
        try self.block();

        const function = Value.from(try self.popCompiler());

        self.vm.stack.push(function);
        const index = try self.currentChunk().addConstant(function);
        _ = self.vm.stack.pop();

        const loc = self.emitOpCode(.{ .closure = .{ .index = index } });

        try self.currentChunk().writeUpvalues(
            compiler.upvalues.items,
            self.previous.line,
        );

        return loc;
    }

    fn emitLoop(self: *Parser, loop_start: usize) Error!usize {
        const offset = self.currentChunk().code.items.len - loop_start + 3;
        if (offset > 0xffff) try self.errorAtPrev("Loop body too large.");

        return self.emitOpCode(.{ .loop = .{ .offset = @truncate(u16, offset) } });
    }

    fn patchJump(self: *Parser, jump_instruction_loc: usize) File.WriteError!void {
        const offset = self.currentChunk().code.items.len - jump_instruction_loc - 3;

        if (offset > 0xffff) try self.errorAtPrev("Too much code to jump over.");

        self.currentChunk().patchU16(@truncate(u16, offset), jump_instruction_loc + 1);
    }

    fn stringConstant(self: *Parser, bytes: []const u8) Allocator.Error!usize {
        if (self
            .current_compiler
            .constant_strings
            .getAdapted(bytes, Object.String.SliceContext{})) |index|
        {
            return index;
        }

        const identifier = try Object.String.createCopy(self.vm, bytes);

        self.vm.stack.push(Value.from(identifier));
        const index = try self.currentChunk().addConstant(identifier);
        _ = self.vm.stack.pop();

        try self.current_compiler.constant_strings.put(identifier, index);
        return index;
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
