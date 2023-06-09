const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const config = @import("config");

const hashBytes = @import("hash.zig").hashBytes;
const Vm = @import("Vm.zig");
const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const GcAllocator = @import("GcAllocator.zig");
const FixedCapacityStack = @import("stack.zig").FixedCapacityStack;

const Object = @This();

typ: Type,
next: ?*Object,
is_marked: bool = false,

pub const Type = enum {
    string,
    upvalue,

    function,
    native,
    closure,

    class,
    instance,
    bound_method,

    fn T(comptime self: Type) type {
        return switch (self) {
            .string => String,
            .upvalue => Upvalue,

            .function => Function,
            .native => Native,
            .closure => Closure,

            .class => Class,
            .instance => Instance,
            .bound_method => BoundMethod,
        };
    }
};

pub const String = struct {
    object: Object,
    bytes: []const u8,
    hash: u64,

    pub fn HashMap(comptime V: type) type {
        return std.HashMap(*String, V, HashContext, std.hash_map.default_max_load_percentage);
    }

    pub const HashContext = struct {
        pub fn hash(_: HashContext, key: *String) u64 {
            return key.hash;
        }

        pub fn eql(_: HashContext, k1: *String, k2: *String) bool {
            return k2.eql(k1);
        }
    };

    pub const SliceContext = struct {
        pub fn hash(_: SliceContext, key: []const u8) u64 {
            return hashBytes(key);
        }

        pub fn eql(_: SliceContext, k1: []const u8, k2: *String) bool {
            return k2.eqlSlice(k1);
        }
    };

    /// Create a new `String` object with ownership of `bytes` transferred. Caller owns the memory.
    /// Call `destroy` with the result to free memory.
    pub fn create(vm: *Vm, bytes: []const u8) Allocator.Error!*String {
        if (tryCreateIntern(vm, bytes)) |out| {
            vm.allocator.free(bytes);
            return out;
        }

        return createAssumeNovel(vm, bytes);
    }

    /// Create a new `String` object by copying `bytes`. Caller owns the memory. Call `destroy`
    /// with the result to free memory.
    pub fn createCopy(vm: *Vm, bytes: []const u8) Allocator.Error!*String {
        if (tryCreateIntern(vm, bytes)) |out| return out;

        const copied_bytes = try vm.allocator.dupe(u8, bytes);
        return createAssumeNovel(vm, copied_bytes);
    }

    fn tryCreateIntern(vm: *Vm, bytes: []const u8) ?*String {
        return vm.strings.getKeyAdapted(bytes, SliceContext{});
    }

    fn createAssumeNovel(vm: *Vm, bytes: []const u8) Allocator.Error!*String {
        const object = try Object.create(vm, .string);
        const out = object.as(.string);

        out.* = .{
            .object = object.*,
            .bytes = bytes,
            .hash = hashBytes(bytes),
        };

        vm.stack.push(Value.from(out));
        try vm.strings.put(out, {});
        _ = vm.stack.pop();

        return out;
    }

    /// Deallocate the `String` object and the inner `bytes`.
    pub fn destroy(self: *String, vm: *Vm) void {
        vm.allocator.free(self.bytes);
        vm.allocator.destroy(self);
    }

    /// Check for equality between *String with simple pointer equality.
    /// This can be done as all `String` instances are interned by `vm.strings`.
    pub fn eql(self: *const String, other: *const String) bool {
        return @ptrToInt(self) == @ptrToInt(other);
    }

    pub fn eqlSlice(self: *const String, bytes: []const u8) bool {
        return mem.eql(u8, self.bytes, bytes);
    }

    pub fn format(
        self: *const String,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;

        if (std.mem.eql(u8, fmt, "#")) try writer.writeAll("\"");
        try writer.print("{s}", .{self.bytes});
        if (std.mem.eql(u8, fmt, "#")) try writer.writeAll("\"");
    }

    test "compare String and a slice" {
        var vm = Vm.create();
        try vm.init(std.testing.allocator);
        defer vm.deinit();

        const string = try String.createCopy(&vm, "hello");
        try std.testing.expect(string.eqlSlice("hello"));
    }

    test "HashMap sanity check" {
        var vm = Vm.create();
        try vm.init(std.testing.allocator);
        defer vm.deinit();

        const string = try String.createCopy(&vm, "hello");
        var hashmap = Object.String.HashMap(void).init(std.testing.allocator);
        defer hashmap.deinit();

        try hashmap.put(string, {});

        {
            const result = hashmap.getKey(string);
            try std.testing.expect(result.?.hash == hashBytes("hello"));
        }

        {
            try std.testing.expect(hashmap.containsAdapted(
                @as([]const u8, "hello"),
                SliceContext{},
            ));

            const result = hashmap.getKeyAdapted(@as([]const u8, "pello"), SliceContext{});
            try std.testing.expect(result == null);
        }
    }
};

pub const Upvalue = struct {
    object: Object,
    location: *Value,
    closed: Value = .nil,
    next: ?*Upvalue,

    pub fn create(vm: *Vm, location: *Value, next: ?*Upvalue) Allocator.Error!*Upvalue {
        const object = try Object.create(vm, .upvalue);
        const out = object.as(.upvalue);

        out.* = .{
            .object = object.*,
            .location = location,
            .next = next,
        };

        return out;
    }

    pub fn destroy(self: *Upvalue, vm: *Vm) void {
        vm.allocator.destroy(self);
    }

    pub fn format(
        self: *const Upvalue,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;

        if (std.mem.eql(u8, fmt, "#")) {
            try writer.print("<upvalue -> {#}>", .{self.location});
        } else {
            try writer.writeAll("<upvalue>");
        }
    }

    pub fn eql(self: *const Upvalue, other: *const Upvalue) bool {
        return @ptrToInt(self) == @ptrToInt(other);
    }
};

pub const Function = struct {
    object: Object,
    arity: u8 = 0,
    chunk: Chunk,
    name: ?*String = null,
    upvalue_count: usize = 0,

    pub fn create(vm: *Vm) Allocator.Error!*Function {
        const object = try Object.create(vm, .function);
        const out = object.as(.function);

        out.* = .{
            .object = object.*,
            .chunk = Chunk.init(vm.allocator),
        };

        return out;
    }

    pub fn destroy(self: *Function, vm: *Vm) void {
        self.chunk.deinit();
        vm.allocator.destroy(self);
    }

    pub fn format(
        self: *const Function,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        if (self.name) |n| {
            try writer.print("<fn {s}>", .{n.bytes});
        } else {
            try writer.writeAll("<script>");
        }
    }

    pub fn eql(self: *const Function, other: *const Function) bool {
        return @ptrToInt(self) == @ptrToInt(other);
    }
};

pub const Native = struct {
    object: Object,
    arity: u8,
    function: Fn,

    pub const Fn = *const fn (vm: *Vm, args: []const Value) Vm.Error!Value;

    pub fn create(vm: *Vm, function: Fn, arity: u8) Allocator.Error!*Native {
        const object = try Object.create(vm, .native);
        const out = object.as(.native);

        out.* = .{
            .object = object.*,
            .arity = arity,
            .function = function,
        };

        return out;
    }

    pub fn destroy(self: *Native, vm: *Vm) void {
        vm.allocator.destroy(self);
    }

    pub fn format(
        self: *const Native,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = self;
        _ = fmt;
        _ = options;

        try writer.writeAll("<native fn>");
    }

    pub fn eql(self: *const Native, other: *const Native) bool {
        return @ptrToInt(self) == @ptrToInt(other);
    }
};

pub const Closure = struct {
    object: Object,
    function: *Function,
    upvalues: []*Upvalue,
    upvalue_count: usize = 0,

    pub fn create(vm: *Vm, function: *Function) Allocator.Error!*Closure {
        const upvalues = try vm.allocator.alloc(*Upvalue, function.upvalue_count);

        const object = try Object.create(vm, .closure);
        const out = object.as(.closure);

        out.* = .{
            .object = object.*,
            .function = function,
            .upvalues = upvalues,
        };

        return out;
    }

    pub fn destroy(self: *Closure, vm: *Vm) void {
        vm.allocator.free(self.upvalues);
        vm.allocator.destroy(self);
    }

    pub fn format(
        self: *const Closure,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (std.mem.eql(u8, fmt, "#")) {
            try writer.print("<closure {#}>", .{self.function});
        } else {
            try self.function.format(fmt, options, writer);
        }
    }

    pub fn eql(self: *const Closure, other: *const Closure) bool {
        return @ptrToInt(self) == @ptrToInt(other);
    }
};

pub const Class = struct {
    object: Object,
    name: *String,
    methods: String.HashMap(*Closure),

    pub fn create(vm: *Vm, name: *String) Allocator.Error!*Class {
        const methods = String.HashMap(*Closure).init(vm.allocator);

        const object = try Object.create(vm, .class);
        const out = object.as(.class);

        out.* = .{
            .object = object.*,
            .name = name,
            .methods = methods,
        };

        return out;
    }

    pub fn destroy(self: *Class, vm: *Vm) void {
        self.methods.deinit();
        vm.allocator.destroy(self);
    }

    pub fn format(
        self: *const Class,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;

        try writer.print("<class {s} instance>", .{self.name.bytes});
    }

    pub fn eql(self: *const Class, other: *const Class) bool {
        return @ptrToInt(self) == @ptrToInt(other);
    }
};

pub const Instance = struct {
    object: Object,
    class: *Class,
    fields: String.HashMap(Value),

    pub fn create(vm: *Vm, class: *Class) Allocator.Error!*Instance {
        const fields = String.HashMap(Value).init(vm.allocator);

        const object = try Object.create(vm, .instance);
        const out = object.as(.instance);

        out.* = .{
            .object = object.*,
            .class = class,
            .fields = fields,
        };

        return out;
    }

    pub fn destroy(self: *Instance, vm: *Vm) void {
        self.fields.deinit();
        vm.allocator.destroy(self);
    }

    pub fn format(
        self: *const Instance,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;

        try writer.print("<class {s} instance>", .{self.class.name.bytes});
    }

    pub fn eql(self: *const Instance, other: *const Instance) bool {
        return @ptrToInt(self) == @ptrToInt(other);
    }
};

pub const BoundMethod = struct {
    object: Object,
    receiver: Value,
    method: *Closure,

    pub fn create(vm: *Vm, receiver: Value, method: *Closure) Allocator.Error!*BoundMethod {
        const object = try Object.create(vm, .bound_method);
        const out = object.as(.bound_method);

        out.* = .{
            .object = object.*,
            .receiver = receiver,
            .method = method,
        };

        return out;
    }

    pub fn destroy(self: *BoundMethod, vm: *Vm) void {
        vm.allocator.destroy(self);
    }

    pub fn format(
        self: *const BoundMethod,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (std.mem.eql(u8, fmt, "#")) {
            try writer.print("<bound method {#} {#} >", .{ self.receiver, Value.from(self.method) });
        } else {
            try self.method.format(fmt, options, writer);
        }
    }

    pub fn eql(self: *const BoundMethod, other: *const BoundMethod) bool {
        return @ptrToInt(self) == @ptrToInt(other);
    }
};

pub fn create(vm: *Vm, comptime typ: Type) Allocator.Error!*Object {
    var object = &(try vm.allocator.create(typ.T())).object;

    if (config.log_gc) {
        GcAllocator.log.debug(
            "alloced {} for {*} .{s}",
            .{ @sizeOf(typ.T()), object, @tagName(typ) },
        );
    }

    const next = vm.objects;
    vm.objects = object;

    object.* = .{ .typ = typ, .next = next };
    return object;
}

pub fn destroy(self: *Object, vm: *Vm) void {
    if (config.log_gc) {
        GcAllocator.log.debug(
            "free {*} .{s}",
            .{ self, @tagName(self.typ) },
        );
    }

    switch (self.typ) {
        .string => self.as(.string).destroy(vm),
        .upvalue => self.as(.upvalue).destroy(vm),

        .function => self.as(.function).destroy(vm),
        .native => self.as(.native).destroy(vm),
        .closure => self.as(.closure).destroy(vm),

        .class => self.as(.class).destroy(vm),
        .instance => self.as(.instance).destroy(vm),
        .bound_method => self.as(.bound_method).destroy(vm),
    }
}

pub fn as(self: *Object, comptime typ: Type) *(typ.T()) {
    std.debug.assert(self.typ == typ);
    return @fieldParentPtr(typ.T(), "object", self);
}

pub fn asConst(self: *const Object, comptime typ: Type) *const (typ.T()) {
    std.debug.assert(self.typ == typ);
    return @fieldParentPtr(typ.T(), "object", self);
}

pub fn eql(self: *const Object, other: *const Object) bool {
    if (self.typ != other.typ) return false;

    return switch (self.typ) {
        .string => self.asConst(.string).eql(other.asConst(.string)),
        .upvalue => self.asConst(.upvalue).eql(other.asConst(.upvalue)),

        .function => self.asConst(.function).eql(other.asConst(.function)),
        .native => self.asConst(.native).eql(other.asConst(.native)),
        .closure => self.asConst(.closure).eql(other.asConst(.closure)),

        .class => self.asConst(.class).eql(other.asConst(.class)),
        .instance => self.asConst(.instance).eql(other.asConst(.instance)),
        .bound_method => self.asConst(.bound_method).eql(other.asConst(.bound_method)),
    };
}

pub fn format(
    self: *const Object,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    if (std.mem.eql(u8, fmt, "#")) try writer.print("{*} .{s} ", .{ self, @tagName(self.typ) });
    return switch (self.typ) {
        .string => self.asConst(.string).format(fmt, options, writer),
        .upvalue => self.asConst(.upvalue).format(fmt, options, writer),

        .function => self.asConst(.function).format(fmt, options, writer),
        .native => self.asConst(.native).format(fmt, options, writer),
        .closure => self.asConst(.closure).format(fmt, options, writer),

        .class => self.asConst(.class).format(fmt, options, writer),
        .instance => self.asConst(.instance).format(fmt, options, writer),
        .bound_method => self.asConst(.bound_method).format(fmt, options, writer),
    };
}
