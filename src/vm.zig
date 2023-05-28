const std = @import("std");
const mem = std.mem;
const io = std.io;
const Allocator = mem.Allocator;
const File = std.fs.File;

const config = @import("config");

const debug = @import("debug.zig");
const InterpretError = @import("root").InterpretError;
const FixedCapacityStack = @import("stack.zig").FixedCapacityStack;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;

pub const Vm = struct {
    const stack_max = 256;

    chunk: *Chunk,
    ip: usize,
    stack: FixedCapacityStack(Value),

    pub fn init(allocator: Allocator) Allocator.Error!Vm {
        return .{
            .chunk = undefined,
            .ip = 0,
            .stack = try FixedCapacityStack(Value).init(allocator, stack_max),
        };
    }

    pub fn deinit(self: *Vm) void {
        self.stack.deinit();
    }

    pub fn interpret(self: *Vm, chunk: *Chunk) InterpretError!void {
        self.chunk = chunk;
        self.ip = 0;
        try self.run();
    }

    fn run(self: *Vm) InterpretError!void {
        while (self.ip < self.chunk.code.items.len) {
            if (config.trace_exec) {
                std.debug.print("            {any}\n", .{self.stack.items()});
                _ = debug.disassembleInstruction(self.chunk, self.ip);
            }

            const instruction = self.chunk.nextOpCode(&self.ip);
            if (instruction) |inst| {
                try self.runInstruction(inst);
            }
        }
    }

    fn runInstruction(self: *Vm, instruction: OpCode) InterpretError!void {
        switch (instruction) {
            .@"return" => std.debug.print("{}\n", .{self.stack.pop()}),

            .constant => |op| self.runConstant(op.offset),
            .constant_long => |op| self.runConstant(op.offset),

            .nil => self.stack.push(.nil),
            .true => self.stack.push(Value.from(true)),
            .false => self.stack.push(Value.from(false)),

            .equal => {
                const b = self.stack.pop();
                const a = self.stack.pop();
                self.stack.push(Value.from(a.equal(b)));
            },
            .greater => {},
            .less => {},

            .not => self.stack.push(Value.from(!self.stack.pop().truthiness())),
            .negate => {
                switch (self.stack.pop()) {
                    .number => |n| self.stack.push(Value.from(-n)),
                    else => {
                        try self.runtimeError("Operand must be a number", .{});
                        return error.RuntimePanic;
                    },
                }
            },
            .add => try self.binaryOp(.@"+"),
            .subtract => try self.binaryOp(.@"-"),
            .multiply => try self.binaryOp(.@"*"),
            .divide => try self.binaryOp(.@"/"),
        }
    }

    fn runConstant(self: *Vm, constant_offset: usize) void {
        self.stack.push(self.chunk.constants.items[constant_offset]);
    }

    fn binaryOp(self: *Vm, comptime op: @Type(.EnumLiteral)) InterpretError!void {
        const b_value = self.stack.pop();
        const a_value = self.stack.pop();

        if (a_value != .number or b_value != .number) {
            try self.runtimeError("Operands must be numbers.", .{});
            return error.RuntimePanic;
        }

        const a = a_value.number;
        const b = b_value.number;

        var res = Value.from(switch (op) {
            .@"+" => a + b,
            .@"-" => a - b,
            .@"*" => a * b,
            .@"/" => a / b,

            .@">" => a > b,
            .@"<" => a < b,

            else => {
                @compileError("Unsupported binary operation: " ++ @tagName(op));
            },
        });

        self.stack.push(res);
    }

    fn runtimeError(self: *Vm, comptime fmt: []const u8, args: anytype) File.WriteError!void {
        const stderr = io.getStdErr().writer();

        try stderr.print(fmt, args);
        try stderr.writeAll("\n");

        const line = self.chunk.getLine(self.ip -| 1);
        try stderr.print("[line {}] in script\n", .{line});

        self.stack.clear();
    }
};
