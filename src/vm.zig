const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const File = std.fs.File;

const config = @import("config");

const debug = @import("debug.zig");
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

    pub fn interpret(self: *Vm, chunk: *Chunk) !void {
        self.chunk = chunk;
        self.ip = 0;
        try self.run();
    }

    fn run(self: *Vm) !void {
        while (self.ip < self.chunk.code.items.len) {
            if (config.trace_exec) {
                std.debug.print("            {any}\n", .{self.stack.items()});
                _ = debug.disassembleInstruction(self.chunk, self.ip);
            }

            const instruction = self.chunk.nextOpCode(&self.ip);
            if (instruction) |inst| {
                self.runInstruction(inst);
            }
        }
    }

    fn runInstruction(self: *Vm, instruction: OpCode) void {
        switch (instruction) {
            .@"return" => std.debug.print("{}\n", .{self.stack.pop()}),

            .constant => |op| self.runConstant(op.offset),
            .constant_long => |op| self.runConstant(op.offset),

            .negate => self.stack.push(.{ .value = -self.stack.pop().value }),

            .add => self.binaryOp(.@"+"),
            .subtract => self.binaryOp(.@"-"),
            .multiply => self.binaryOp(.@"*"),
            .divide => self.binaryOp(.@"/"),
        }
    }

    fn runConstant(self: *Vm, constant_offset: usize) void {
        self.stack.push(self.chunk.constants.items[constant_offset]);
    }

    fn binaryOp(self: *Vm, comptime op: @Type(.EnumLiteral)) void {
        const b = self.stack.pop().value;
        const a = self.stack.pop().value;

        var res: f64 = undefined;
        switch (op) {
            .@"+" => res = a + b,
            .@"-" => res = a - b,
            .@"*" => res = a * b,
            .@"/" => res = a / b,

            else => {
                @compileError("Unsupported binary operation: " ++ @tagName(op));
            },
        }

        self.stack.push(Value.from(res));
    }
};
