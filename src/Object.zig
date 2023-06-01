const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

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
    hash: u32,

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
        return vm.strings.getKey(bytes);
    }

    fn createAssumeNovel(vm: *Vm, bytes: []const u8) Allocator.Error!*String {
        const object = try Object.create(vm, .string);
        var out = object.asString();

        out.* = .{
            .object = object.*,
            .bytes = bytes,
            .hash = Table.hash(bytes),
        };

        _ = try vm.strings.put(out, .nil);

        return out;
    }

    /// Deallocate the `String` object and the inner `bytes`.
    pub fn destroy(self: *String, vm: *Vm) void {
        vm.allocator.free(self.bytes);
        vm.allocator.destroy(self);
    }

    pub fn eql(self: *const String, other: anytype) bool {
        const Other = @TypeOf(other);
        switch (Other) {
            *const Object, *Object, *const String, *String => {
                return @ptrToInt(self) == @ptrToInt(other);
            },

            []const u8, []u8 => return mem.eql(u8, self.bytes, other),

            else => {
                @compileError("Cannot compare " ++ @typeName(String) ++ " with " ++
                    @typeName(Other) ++ ".");
            },
        }
        if (Other == *const Object or Other == *Object) {
            return mem.eql(u8, self.bytes, other.asString().bytes);
        } else if (Other == *const String or Other == *String) {
            return mem.eql(u8, self.bytes, other.bytes);
        } else if (Other == []const u8 or Other == []u8) {} else {}
    }

    pub fn format(
        self: *String,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;

        if (std.mem.eql(u8, fmt, "#")) try writer.writeAll("\"");
        try writer.print("{s}", .{self.bytes});
        if (std.mem.eql(u8, fmt, "#")) try writer.writeAll("\"");
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

pub fn eql(self: *Object, other: anytype) bool {
    if (@TypeOf(other) == *const Object or @TypeOf(other) == *const Object) {
        if (self.typ != other.typ) return false;
    }

    return switch (self.typ) {
        .string => self.asString().eql(other),
    };
}

pub fn format(
    self: *Object,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    if (std.mem.eql(u8, fmt, "#")) try writer.print("{*} ", .{self});
    switch (self.typ) {
        .string => try self.asString().format(fmt, options, writer),
    }
}
