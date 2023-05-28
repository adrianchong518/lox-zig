const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const Vm = @import("vm.zig").Vm;

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

    /// Create a new `String` object with ownership of `bytes` transferred. Caller owns the memory.
    /// Call `destroy` with the result to free memory.
    pub fn create(vm: *Vm, bytes: []const u8) Allocator.Error!*String {
        const object = try Object.create(vm, .string);
        var out = object.asString();
        out.* = .{
            .object = object.*,
            .bytes = bytes,
        };
        return out;
    }

    /// Create a new `String` object by copying `bytes`. Caller owns the memory. Call `destroy`
    /// with the result to free memory.
    pub fn createCopy(vm: *Vm, bytes: []const u8) Allocator.Error!*String {
        const copied_bytes = try vm.allocator.dupe(u8, bytes);
        return String.create(vm, copied_bytes);
    }

    /// Deallocate the `String` object and the inner `bytes`.
    pub fn destroy(self: *String, vm: *Vm) void {
        vm.allocator.free(self.bytes);
        vm.allocator.destroy(self);
    }

    fn equal(self: *const String, other: *const String) bool {
        return mem.eql(u8, self.bytes, other.bytes);
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

pub fn isString(self: *const Object) bool {
    return self.typ == .string;
}

pub fn asString(self: *Object) *String {
    std.debug.assert(self.isString());
    return @fieldParentPtr(String, "object", self);
}

pub fn equal(self: *const Object, other: *const Object) bool {
    if (self.typ != other.typ) return false;

    return switch (self.typ) {
        .string => self.asString().equal(other.asString()),
    };
}

pub fn format(
    self: *Object,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;

    switch (self.typ) {
        .string => try writer.print("{s}", .{self.asString().bytes}),
    }
}
