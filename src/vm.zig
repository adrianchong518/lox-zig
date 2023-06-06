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

    stack: FixedCapacityStack(Value),
    strings: Object.String.HashMap(void),
    globals: Object.String.HashMap(Value),

    frames: FixedCapacityStack(CallFrame),

    objects: ?*Object,

    const stack_max = frames_max * (std.math.maxInt(u8) + 1);
    const frames_max = 64;

    const Error = File.WriteError || Allocator.Error || error{RuntimePanic};
    const Status = ?enum { stop };

    const CallFrame = struct {
        function: *Object.Function,
        ip: usize = 0,
        slots_start: usize,

        fn chunk(self: CallFrame) Chunk {
            return self.function.chunk;
        }

        fn slots(self: CallFrame, vm: *Vm) []Value {
            return vm.stack.items()[self.slots_start..];
        }
    };

    pub fn init(allocator: Allocator) Allocator.Error!Vm {
        var vm = Vm{
            .allocator = allocator,

            .stack = try FixedCapacityStack(Value).init(allocator, stack_max),
            .strings = Object.String.HashMap(void).init(allocator),
            .globals = Object.String.HashMap(Value).init(allocator),

            .frames = try FixedCapacityStack(CallFrame).init(allocator, frames_max),

            .objects = null,
        };

        try vm.defineNative("clock", clockNative);

        return vm;
    }

    pub fn deinit(self: *Vm) void {
        self.frames.deinit();

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

    pub fn interpret(self: *Vm, function: *Object.Function) Error!void {
        self.stack.push(Value.from(function));
        try self.call(function, 0);

        try self.run();
    }

    fn run(self: *Vm) Error!void {
        while (true) {
            const frame = self.frames.peekPtr(0);
            if (config.trace_exec) {
                std.debug.print("           ", .{});
                for (self.stack.items()) |i| {
                    std.debug.print(" [ {#} ]", .{i});
                }
                std.debug.print("\n", .{});
                _ = debug.disassembleInstruction(frame.chunk(), frame.ip);
            }

            const instruction = frame.chunk().nextOpCode(&frame.ip);
            if (instruction) |inst| {
                if (try self.runInstruction(inst, frame)) |s| {
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

    fn runInstruction(self: *Vm, instruction: OpCode, frame: *CallFrame) Error!Status {
        switch (instruction) {
            .call => |op| {
                const callee = self.stack.peek(op.arg_count);
                try self.callValue(callee, op.arg_count);
            },

            .@"return" => {
                const result = self.stack.pop();
                _ = self.frames.pop();
                if (self.frames.count() == 0) {
                    _ = self.stack.pop();
                    return .stop;
                }

                self.stack.discardUntil(frame.slots_start);
                self.stack.push(result);
            },

            .constant => |op| self.stack.push(frame.chunk().constants.items[op.offset]),

            .define_global => |op| {
                const name = frame.chunk().constants.items[op.offset].object.as(.string);
                _ = try self.globals.put(name, self.stack.peek(0));
                _ = self.stack.pop();
            },
            .get_global => |op| {
                const name = frame.chunk().constants.items[op.offset].object.as(.string);
                const value = self.globals.get(name) orelse {
                    try self.runtimeError("Undefined variable '{0}' ({0#})", .{Value.from(name)});
                    return error.RuntimePanic;
                };
                self.stack.push(value);
            },
            .set_global => |op| {
                const name = frame.chunk().constants.items[op.offset].object.as(.string);
                const value_ptr = self.globals.getPtr(name) orelse {
                    try self.runtimeError("Undefined variable '{0}' ({0#})", .{Value.from(name)});
                    return error.RuntimePanic;
                };
                const value = self.stack.peek(0);
                value_ptr.* = value;
            },

            .get_local => |op| {
                const value = frame.slots(self)[op.offset];
                self.stack.push(value);
            },
            .set_local => |op| {
                const value = self.stack.peek(0);
                frame.slots(self)[op.offset] = value;
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
            .greater => try self.binaryOp(.@">"),
            .less => try self.binaryOp(.@"<"),

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

                if (a_value.isObjectType(.string) and b_value.isObjectType(.string)) {
                    const bytes = try mem.concat(self.allocator, u8, &[_][]const u8{
                        a_value.object.as(.string).bytes,
                        b_value.object.as(.string).bytes,
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

            .jump => |op| {
                frame.ip += op.offset;
            },
            .jump_if_false => |op| {
                if (!self.stack.peek(0).truthiness()) frame.ip += op.offset;
            },
            .loop => |op| {
                frame.ip -= op.offset;
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

    fn callValue(self: *Vm, callee: Value, arg_count: u8) Error!void {
        if (callee == .object) {
            switch (callee.object.typ) {
                .function => return try self.call(callee.object.as(.function), arg_count),
                .native => {
                    const native = callee.object.as(.native).function;
                    const args_start = self.stack.peekIndex(arg_count) + 1;
                    const args = self.stack.items()[args_start..][0..arg_count];
                    const result = native(args);

                    self.stack.discardUntil(args_start - 1);
                    self.stack.push(result);

                    return;
                },
                else => {},
            }
        }

        try self.runtimeError(
            "Can only call functions and classes, but got: {s}",
            .{@tagName(callee)},
        );
        return error.RuntimePanic;
    }

    fn call(self: *Vm, function: *Object.Function, arg_count: u8) Error!void {
        if (arg_count != function.arity) {
            try self.runtimeError(
                "Expected {} arguments but got {}",
                .{ function.arity, arg_count },
            );
            return error.RuntimePanic;
        }

        self.frames.tryPush(.{
            .function = function,
            .slots_start = self.stack.peekIndex(arg_count),
        }) catch {
            try self.runtimeError("Stack overflow.", .{});
            return error.RuntimePanic;
        };
    }

    fn runtimeError(self: *Vm, comptime fmt: []const u8, args: anytype) File.WriteError!void {
        const stderr = io.getStdErr().writer();

        try stderr.print(fmt, args);
        try stderr.writeAll("\n");

        while (self.frames.tryPop()) |frame| {
            const function = frame.function;
            const line = frame.chunk().getLine(frame.ip -| 1);
            if (frame.function.name) |n| {
                try stderr.print("[line {}] in {s}()\n", .{ line, n });
            } else {
                try stderr.print("[line {}] in script\n", .{line});
            }
            _ = function;
        }

        self.stack.clear();
    }

    fn defineNative(self: *Vm, name: []const u8, function: Object.Native.Fn) Allocator.Error!void {
        self.stack.push(Value.from(try Object.String.createCopy(self, name)));
        self.stack.push(Value.from(try Object.Native.create(self, function)));

        try self.globals.put(self.stack.peek(1).object.as(.string), self.stack.peek(0));

        _ = self.stack.pop();
        _ = self.stack.pop();
    }
};

fn clockNative(_: []Value) Value {
    return Value.from(@intToFloat(f64, std.time.microTimestamp()) / std.time.us_per_s);
}
