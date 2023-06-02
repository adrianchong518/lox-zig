const std = @import("std");
const mem = std.mem;
const io = std.io;
const Allocator = mem.Allocator;
const ArrayList = std.ArrayList;
const File = std.fs.File;

const config = @import("config");

const debug = @import("debug.zig");
const InterpretError = @import("root").InterpretError;
const FixedCapacityStack = @import("stack.zig").FixedCapacityStack;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Object = @import("Object.zig");

pub const Vm = struct {
    allocator: Allocator,

    chunk: *Chunk,
    ip: usize,

    stack: FixedCapacityStack(Value),
    strings: Object.String.HashMap(void),
    globals: Object.String.HashMap(Value),

    objects: ?*Object,

    const stack_max = 256;

    const Error = File.WriteError || Allocator.Error || error{RuntimePanic};
    const Status = ?enum { stop };

    pub fn init(allocator: Allocator) Allocator.Error!Vm {
        return .{
            .allocator = allocator,

            .chunk = undefined,
            .ip = 0,

            .stack = try FixedCapacityStack(Value).init(allocator, stack_max),
            .strings = Object.String.HashMap(void).init(allocator),
            .globals = Object.String.HashMap(Value).init(allocator),

            .objects = null,
        };
    }

    pub fn deinit(self: *Vm) void {
        self.globals.deinit();
        self.strings.deinit();
        self.stack.deinit();

        var object = self.objects;
        while (object) |obj| {
            const next = obj.next;
            obj.destroy(self);
            object = next;
        }

        self.* = undefined;
    }

    pub fn interpret(self: *Vm, chunk: *Chunk) Error!void {
        self.chunk = chunk;
        self.ip = 0;
        try self.run();
    }

    fn run(self: *Vm) Error!void {
        while (true) {
            if (config.trace_exec) {
                std.debug.print("           ", .{});
                for (self.stack.items()) |i| {
                    std.debug.print(" [ {#} ]", .{i});
                }
                std.debug.print("\n", .{});
                _ = debug.disassembleInstruction(self.chunk.*, self.ip);
            }

            const instruction = self.chunk.nextOpCode(&self.ip);
            if (instruction) |inst| {
                if (try self.runInstruction(inst)) |s| {
                    switch (s) {
                        .stop => {
                            std.debug.assert(self.stack.isEmpty());
                            return;
                        },
                    }
                }
            }
        }
    }

    fn runInstruction(self: *Vm, instruction: OpCode) Error!Status {
        switch (instruction) {
            .@"return" => return .stop,

            .constant => |op| self.stack.push(self.chunk.constants.items[op.offset]),

            .define_global => |op| {
                const name = self.chunk.constants.items[op.offset].object.asString();
                _ = try self.globals.put(name, self.stack.peek(0));
                _ = self.stack.pop();
            },
            .get_global => |op| {
                const name = self.chunk.constants.items[op.offset].object.asString();
                const value = self.globals.get(name) orelse {
                    try self.runtimeError("Undefined variable '{0}' ({0#})", .{Value.from(name)});
                    return error.RuntimePanic;
                };
                self.stack.push(value);
            },
            .set_global => |op| {
                const name = self.chunk.constants.items[op.offset].object.asString();
                const value_ptr = self.globals.getPtr(name) orelse {
                    try self.runtimeError("Undefined variable '{0}' ({0#})", .{Value.from(name)});
                    return error.RuntimePanic;
                };
                const value = self.stack.peek(0);
                value_ptr.* = value;
            },

            .get_local => |op| {
                const value = self.stack.items()[op.offset];
                self.stack.push(value);
            },
            .set_local => |op| {
                const value = self.stack.peek(0);
                self.stack.items()[op.offset] = value;
            },

            .pop => _ = self.stack.pop(),

            .nil => self.stack.push(.nil),
            .true => self.stack.push(Value.from(true)),
            .false => self.stack.push(Value.from(false)),

            .equal => {
                const b = self.stack.pop();
                const a = self.stack.pop();
                self.stack.push(Value.from(a.eql(b)));
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

            .add => {
                const b_value = self.stack.pop();
                const a_value = self.stack.pop();

                if (a_value.isString() and b_value.isString()) {
                    const bytes = try mem.concat(self.allocator, u8, &[_][]const u8{
                        a_value.object.asString().bytes,
                        b_value.object.asString().bytes,
                    });
                    errdefer self.allocator.free(bytes);

                    const object = try Object.String.create(self, bytes);
                    self.stack.push(Value.from(object));
                } else if (a_value == .number and b_value == .number) {
                    self.stack.push(Value.from(a_value.number + b_value.number));
                } else {
                    try self.runtimeError("Operands must be either both numbers or strings.", .{});
                    return error.RuntimePanic;
                }
            },

            .subtract => try self.binaryOp(.@"-"),
            .multiply => try self.binaryOp(.@"*"),
            .divide => try self.binaryOp(.@"/"),

            .print => {
                const value = self.stack.pop();
                try io.getStdOut().writer().print("{}\n", .{value});
                if (config.trace_exec) std.debug.print("{#}\n", .{value});
            },
        }

        return null;
    }

    fn binaryOp(self: *Vm, comptime op: @Type(.EnumLiteral)) Error!void {
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
