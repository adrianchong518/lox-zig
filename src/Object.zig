const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const hashBytes = @import("hash.zig").hashBytes;
const Vm = @import("vm.zig").Vm;
const Table = @import("Table.zig");

const Object = @This();

typ: Type,
next: ?*Object,

pub const Type = enum {
    string,

    fn T(comptime self: Type) type {
        return switch (self) {
            .string => String,
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
        var out = object.asString();

        out.* = .{
            .object = object.*,
            .bytes = bytes,
            .hash = hashBytes(bytes),
        };

        try vm.strings.put(out, {});

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
        var vm = try Vm.init(std.testing.allocator);
        defer vm.deinit();

        const string = try String.createCopy(&vm, "hello");
        try std.testing.expect(string.eqlSlice("hello"));
    }

    test "HashMap sanity check" {
        var vm = try Vm.init(std.testing.allocator);
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

pub fn create(vm: *Vm, typ: Type) Allocator.Error!*Object {
    var object = &(try vm.allocator.create(typ.T())).object;

    const next = vm.objects;
    vm.objects = object;

    object.* = .{ .typ = typ, .next = next };
    return object;
}

pub fn destroy(self: *Object, vm: *Vm) void {
    switch (self.typ) {
        .string => self.asString().destroy(vm),
    }
}

pub fn isString(self: Object) bool {
    return self.typ == .string;
}

pub fn asString(self: *Object) *String {
    std.debug.assert(self.isString());
    return @fieldParentPtr(String, "object", self);
}

pub fn asConstString(self: *const Object) *const String {
    std.debug.assert(self.isString());
    return @fieldParentPtr(String, "object", self);
}

pub fn eql(self: *const Object, other: *const Object) bool {
    return switch (self.typ) {
        .string => self.asConstString().eql(other.asConstString()),
    };
}

pub fn format(
    self: *const Object,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    if (std.mem.eql(u8, fmt, "#")) try writer.print("{*} ", .{self});
    switch (self.typ) {
        .string => try self.asConstString().format(fmt, options, writer),
    }
}
