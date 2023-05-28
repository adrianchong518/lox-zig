const std = @import("std");

const Object = @import("Object.zig");

pub const Value = union(enum) {
    nil,
    number: f64,
    bool: bool,
    object: *Object,

    pub fn from(value: anytype) Value {
        const T = @TypeOf(value);
        const info = @typeInfo(T);

        if (info == .Pointer) {
            const Child = info.Pointer.child;
            if (@typeInfo(Child) == .Struct and
                @hasField(Child, "object") and
                @TypeOf(value.object) == Object)
            {
                return .{ .object = &value.object };
            }
        }

        return switch (T) {
            Value => value,
            f64, comptime_float => .{ .number = value },
            bool => .{ .bool = value },
            *Object => .{ .object = value },
            else => @compileError("Unsupported value type: " ++ @typeName(T)),
        };
    }

    pub fn truthiness(self: Value) bool {
        return switch (self) {
            .nil => false,
            .number => true,
            .bool => |b| b,
            .object => @panic("todo"),
        };
    }

    pub fn equal(self: Value, other: Value) bool {
        if (@enumToInt(self) != @enumToInt(other)) return false;

        return switch (self) {
            .nil => true,
            .number => |n| n == other.number,
            .bool => |b| b == other.bool,
            .object => @panic("todo"),
        };
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .nil => try writer.writeAll("nil"),
            .number => |n| try writer.print("{}", .{n}),
            .bool => |b| try writer.print("{}", .{b}),
            .object => |o| try o.format(fmt, options, writer),
        }
    }

    pub fn isString(self: Value) bool {
        return switch (self) {
            .object => |o| o.isString(),
            else => false,
        };
    }
};
