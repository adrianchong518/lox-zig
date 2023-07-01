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
const GcAllocator = @import("GcAllocator.zig");

const Vm = @This();

allocator: Allocator,
gc_allocator: GcAllocator,

stack: FixedCapacityStack(Value),
strings: Object.String.HashMap(void),
globals: Object.String.HashMap(Value),

frames: FixedCapacityStack(CallFrame),

init_string: ?*Object.String = null,
objects: ?*Object = null,
open_upvalues: ?*Object.Upvalue = null,

const stack_size = frames_size * 256;
const frames_size = 64;

pub const Error = File.WriteError || Allocator.Error || error{RuntimePanic};
const Status = ?enum { stop };

const CallFrame = struct {
    closure: *Object.Closure,
    ip: usize = 0,
    slots_start: usize,

    fn chunk(self: CallFrame) Chunk {
        return self.closure.function.chunk;
    }

    fn slots(self: CallFrame, vm: *Vm) []Value {
        return vm.stack.items()[self.slots_start..];
    }
};

pub fn create() Vm {
    return .{
        .allocator = undefined,
        .gc_allocator = undefined,
        .stack = undefined,
        .strings = undefined,
        .globals = undefined,
        .frames = undefined,
    };
}

pub fn init(self: *Vm, backing_allocator: Allocator) Allocator.Error!void {
    self.gc_allocator = GcAllocator.init(backing_allocator, self);
    const allocator = self.gc_allocator.allocator();

    self.allocator = allocator;

    self.stack = try FixedCapacityStack(Value).init(allocator, stack_size);
    self.strings = Object.String.HashMap(void).init(allocator);
    self.globals = Object.String.HashMap(Value).init(allocator);

    self.frames = try FixedCapacityStack(CallFrame).init(allocator, frames_size);

    self.init_string = try Object.String.createCopy(self, "init");

    try self.defineNative("clock", clockNative, 0);
}

pub fn deinit(self: *Vm) void {
    self.frames.deinit(self.allocator);

    self.globals.deinit();
    self.strings.deinit();
    self.stack.deinit(self.allocator);

    self.init_string = null;

    var object = self.objects;
    while (object) |obj| {
        const next = obj.next;
        obj.destroy(self);
        object = next;
    }

    self.gc_allocator.deinit();

    self.* = undefined;
}

pub fn interpret(self: *Vm, function: *Object.Function) Error!void {
    self.stack.push(Value.from(function));
    const script = try Object.Closure.create(self, function);
    self.stack.peekPtr(0).* = Value.from(script);

    try self.call(script, 0);
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

        .invoke => |op| {
            const name = frame.chunk().constants.items[op.index].object.as(.string);
            try self.invoke(name, op.arg_count);
        },

        .@"return" => {
            const result = self.stack.pop();
            self.closeUpvalue(&frame.slots(self)[0]);

            _ = self.frames.pop();
            if (self.frames.count() == 0) {
                _ = self.stack.pop();
                return .stop;
            }

            self.stack.discardUntil(frame.slots_start);
            self.stack.push(result);
        },

        .constant => |op| self.stack.push(frame.chunk().constants.items[op.index]),

        .closure => |op| {
            const function = frame.chunk().constants.items[op.index].object.as(.function);
            const closure = try Object.Closure.create(self, function);
            self.stack.push(Value.from(closure));

            for (closure.upvalues) |*u| {
                const upvalue = frame.chunk().nextUpvalue(&frame.ip);
                if (upvalue.locality == .local) {
                    u.* = try self.captureUpvalue(&frame.slots(self)[upvalue.index]);
                } else {
                    u.* = frame.closure.upvalues[upvalue.index];
                }
                closure.upvalue_count += 1;
            }

            std.debug.assert(function.upvalue_count == closure.upvalue_count);
        },

        .define_global => |op| {
            const name = frame.chunk().constants.items[op.index].object.as(.string);
            _ = try self.globals.put(name, self.stack.peek(0));
            _ = self.stack.pop();
        },
        .get_global => |op| {
            const name = frame.chunk().constants.items[op.index].object.as(.string);
            const value = self.globals.get(name) orelse {
                try self.runtimeError("Undefined variable '{0}' ({0#})", .{Value.from(name)});
                return error.RuntimePanic;
            };
            self.stack.push(value);
        },
        .set_global => |op| {
            const name = frame.chunk().constants.items[op.index].object.as(.string);
            const value_ptr = self.globals.getPtr(name) orelse {
                try self.runtimeError("Undefined variable '{0}' ({0#})", .{Value.from(name)});
                return error.RuntimePanic;
            };
            const value = self.stack.peek(0);
            value_ptr.* = value;
        },

        .get_local => |op| {
            const value = frame.slots(self)[op.index];
            self.stack.push(value);
        },
        .set_local => |op| {
            const value = self.stack.peek(0);
            frame.slots(self)[op.index] = value;
        },

        .get_upvalue => |op| {
            const value = frame.closure.upvalues[op.index].location.*;
            self.stack.push(value);
        },
        .set_upvalue => |op| {
            const value = self.stack.peek(0);
            frame.closure.upvalues[op.index].location.* = value;
        },

        .get_property => |op| {
            const instance = instance: {
                const value = self.stack.peek(0);
                if (!value.isObjectType(.instance)) {
                    try self.runtimeError(
                        "Only instances have properties, but got {#}",
                        .{Value.from(value)},
                    );
                    return error.RuntimePanic;
                }
                break :instance value.object.as(.instance);
            };
            const name = frame.chunk().constants.items[op.index].object.as(.string);

            if (instance.fields.get(name)) |value| {
                _ = self.stack.pop(); // instance
                self.stack.push(value);
                return null;
            }

            try self.bindMethod(Value.from(instance), instance.class, name);
        },
        .set_property => |op| {
            const instance = instance: {
                const value = self.stack.peek(1);
                if (!value.isObjectType(.instance)) {
                    try self.runtimeError("Only instances have properties, but got {#}", .{value});
                    return error.RuntimePanic;
                }
                break :instance value.object.as(.instance);
            };
            const value = self.stack.peek(0);
            const name = frame.chunk().constants.items[op.index].object.as(.string);

            try instance.fields.put(name, value);

            _ = self.stack.pop();
            _ = self.stack.pop();
            self.stack.push(value);
        },

        .class => |op| {
            const name = frame.chunk().constants.items[op.index].object.as(.string);
            const class = try Object.Class.create(self, name);
            self.stack.push(Value.from(class));
        },
        .method => |op| {
            const name = frame.chunk().constants.items[op.index].object.as(.string);
            try self.defineMethod(name);
        },
        .inherit => {
            const superclass = superclass: {
                const value = self.stack.peek(1);
                if (!value.isObjectType(.class)) {
                    try self.runtimeError("Superclass must be a class, but got {#}", .{value});
                    return error.RuntimePanic;
                }
                break :superclass value.object.as(.class);
            };

            const subclass = self.stack.peek(0).object.as(.class);

            // This is safe as `subclass.methods` must be empty when OP_INHERIT is ran
            subclass.methods = try superclass.methods.clone();

            _ = self.stack.pop();
        },
        .get_super => |op| {
            const name = frame.chunk().constants.items[op.index].object.as(.string);
            const superclass = self.stack.pop().object.as(.class);
            const this = self.stack.peek(0);

            try self.bindMethod(this, superclass, name);
        },
        .super_invoke => |op| {
            const name = frame.chunk().constants.items[op.index].object.as(.string);
            const superclass = self.stack.pop().object.as(.class);
            try self.invokeFromClass(superclass, name, op.arg_count);
        },

        .pop => _ = self.stack.pop(),
        .close_upvalue => {
            self.closeUpvalue(self.stack.peekPtr(0));
            _ = self.stack.pop();
        },

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
            const b_value = self.stack.peek(0);
            const a_value = self.stack.peek(1);

            const value = if (a_value.isObjectType(.string) and
                b_value.isObjectType(.string))
            str: {
                const bytes = try mem.concat(self.allocator, u8, &[_][]const u8{
                    a_value.object.as(.string).bytes,
                    b_value.object.as(.string).bytes,
                });
                errdefer self.allocator.free(bytes);

                const object = try Object.String.create(self, bytes);
                break :str Value.from(object);
            } else if (a_value == .number and b_value == .number)
                Value.from(a_value.number + b_value.number)
            else {
                try self.runtimeError("Operands must be either both numbers or strings.", .{});
                return error.RuntimePanic;
            };

            _ = self.stack.pop();
            _ = self.stack.pop();
            self.stack.push(value);
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
            .closure => return self.call(callee.object.as(.closure), arg_count),

            .native => {
                const native = callee.object.as(.native);

                if (arg_count != native.arity) {
                    try self.runtimeError(
                        "Expected {} arguments but got {}",
                        .{ native.arity, arg_count },
                    );
                    return error.RuntimePanic;
                }

                const args_start = self.stack.peekIndex(arg_count) + 1;
                const args = self.stack.items()[args_start..][0..arg_count];

                const result = try native.function(self, args);

                self.stack.discardUntil(args_start - 1);
                self.stack.push(result);

                return;
            },

            .class => {
                const class = callee.object.as(.class);
                const instance = try Object.Instance.create(self, class);
                self.stack.peekPtr(arg_count).* = Value.from(instance);

                if (class.methods.get(self.init_string.?)) |initializer| {
                    return self.call(initializer, arg_count);
                }

                if (arg_count != 0) {
                    try self.runtimeError("Expected 0 arguments but got {}", .{arg_count});
                    return error.RuntimePanic;
                }

                return;
            },

            .bound_method => {
                const bound_method = callee.object.as(.bound_method);
                self.stack.peekPtr(arg_count).* = bound_method.receiver;
                return self.call(bound_method.method, arg_count);
            },

            else => {},
        }
    }

    switch (callee) {
        .object => |obj| {
            try self.runtimeError(
                "Can only call functions and classes, but got: object.{s}",
                .{@tagName(obj.typ)},
            );
        },
        else => {
            try self.runtimeError(
                "Can only call functions and classes, but got: {s}",
                .{@tagName(callee)},
            );
        },
    }

    return error.RuntimePanic;
}

fn call(self: *Vm, closure: *Object.Closure, arg_count: u8) Error!void {
    if (arg_count != closure.function.arity) {
        try self.runtimeError(
            "Expected {} arguments but got {}",
            .{ closure.function.arity, arg_count },
        );
        return error.RuntimePanic;
    }

    self.frames.tryPush(.{
        .closure = closure,
        .slots_start = self.stack.peekIndex(arg_count),
    }) catch {
        try self.runtimeError("Stack overflow.", .{});
        return error.RuntimePanic;
    };
}

fn invoke(self: *Vm, name: *Object.String, arg_count: u8) Error!void {
    const instance = instance: {
        const value = self.stack.peek(arg_count);
        if (!value.isObjectType(.instance)) {
            try self.runtimeError(
                "Only instances have methods, but got {#}",
                .{Value.from(value)},
            );
            return error.RuntimePanic;
        }
        break :instance value.object.as(.instance);
    };

    if (instance.fields.get(name)) |value| {
        self.stack.peekPtr(arg_count).* = value;
        return self.callValue(value, arg_count);
    }

    try self.invokeFromClass(instance.class, name, arg_count);
}

fn invokeFromClass(
    self: *Vm,
    class: *Object.Class,
    name: *Object.String,
    arg_count: u8,
) Error!void {
    if (class.methods.get(name)) |method| {
        return self.call(method, arg_count);
    }

    try self.runtimeError("Undefined property '{}'.", .{name});
    return error.RuntimePanic;
}

fn captureUpvalue(self: *Vm, local: *Value) Allocator.Error!*Object.Upvalue {
    var prev_upvalue: ?*Object.Upvalue = null;
    var upvalue = self.open_upvalues;

    while (upvalue) |u| : ({
        prev_upvalue = upvalue;
        upvalue = u.next;
    }) {
        if (@ptrToInt(u.location) <= @ptrToInt(local)) break;
    }

    if (upvalue) |u| {
        if (u.location == local) return u;
    }

    const new_upvalue = try Object.Upvalue.create(self, local, upvalue);
    if (prev_upvalue) |pu| {
        pu.next = new_upvalue;
    } else {
        self.open_upvalues = new_upvalue;
    }
    return new_upvalue;
}

fn closeUpvalue(self: *Vm, last: *Value) void {
    while (self.open_upvalues) |u| : (self.open_upvalues = u.next) {
        if (@ptrToInt(u.location) < @ptrToInt(last)) break;

        u.closed = u.location.*;
        u.location = &u.closed;
    }
}

fn defineMethod(self: *Vm, name: *Object.String) Allocator.Error!void {
    const method = self.stack.peek(0).object.as(.closure);
    const class = self.stack.peek(1).object.as(.class);
    try class.methods.put(name, method);
    _ = self.stack.pop();
}

fn bindMethod(self: *Vm, receiver: Value, class: *Object.Class, name: *Object.String) Error!void {
    if (class.methods.get(name)) |method| {
        const bound = try Object.BoundMethod.create(self, receiver, method);
        _ = self.stack.pop(); // instance
        self.stack.push(Value.from(bound));
        return;
    }

    try self.runtimeError("Undefined property '{0}' ({0#})", .{Value.from(name)});
    return error.RuntimePanic;
}

fn runtimeError(self: *Vm, comptime fmt: []const u8, args: anytype) File.WriteError!void {
    const stderr = io.getStdErr().writer();

    try stderr.print(fmt, args);
    try stderr.writeAll("\n");

    while (self.frames.tryPop()) |frame| {
        const function = frame.closure.function;
        const line = frame.chunk().getLine(frame.ip -| 1);
        if (function.name) |n| {
            try stderr.print("[line {}] in {s}()\n", .{ line, n });
        } else {
            try stderr.print("[line {}] in script\n", .{line});
        }
    }

    self.stack.clear();
}

fn defineNative(
    self: *Vm,
    name: []const u8,
    function: Object.Native.Fn,
    arity: u8,
) Allocator.Error!void {
    self.stack.push(Value.from(try Object.String.createCopy(self, name)));
    self.stack.push(Value.from(try Object.Native.create(self, function, arity)));

    try self.globals.put(self.stack.peek(1).object.as(.string), self.stack.peek(0));

    _ = self.stack.pop();
    _ = self.stack.pop();
}

fn clockNative(_: *Vm, _: []const Value) Vm.Error!Value {
    return Value.from(@intToFloat(f64, std.time.microTimestamp()) / std.time.us_per_s);
}
