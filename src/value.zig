const std = @import("std");
const trait = std.meta.trait;

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

        return switch (@typeInfo(T)) {
            .Float, .ComptimeFloat => .{ .number = value },
            .Int, .ComptimeInt => .{ .number = @intToFloat(f64, value) },
            else => {
                return switch (T) {
                    Value => value,
                    bool => .{ .bool = value },
                    *Object => .{ .object = value },
                    else => @as(Value, value),
                };
            },
        };
    }

    pub fn truthiness(self: Value) bool {
        return switch (self) {
            .nil => false,
            .bool => |b| b,
            else => true,
        };
    }

    pub fn eql(self: Value, other: Value) bool {
        if (@enumToInt(self) != @enumToInt(other)) return false;

        return switch (self) {
            .nil => true,
            .number => |n| n == other.number,
            .bool => |b| b == other.bool,
            .object => |o| o.eql(other.object),
        };
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (std.mem.eql(u8, fmt, "#")) try writer.print("<.{s} ", .{@tagName(self)});

        switch (self) {
            .nil => try writer.writeAll("nil"),
            .number => |n| try writer.print("{}", .{n}),
            .bool => |b| try writer.print("{}", .{b}),
            .object => |o| try o.format(fmt, options, writer),
        }

        if (std.mem.eql(u8, fmt, "#")) try writer.writeAll(">");
    }

    pub fn isString(self: Value) bool {
        return switch (self) {
            .object => |o| o.isString(),
            else => false,
        };
    }
};
