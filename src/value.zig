const std = @import("std");

pub const Value = union(enum) {
    nil,
    number: f64,
    bool: bool,

    pub fn from(value: anytype) Value {
        const T = @TypeOf(value);

        if (T == Value) {
            return value;
        } else if (T == f64 or T == comptime_float) {
            return .{ .number = value };
        } else if (T == bool) {
            return .{ .bool = value };
        } else {
            @compileError("Unsupported value type: " ++ @typeName(T));
        }
    }

    pub fn truthiness(self: Value) bool {
        return switch (self) {
            .nil => false,
            .number => true,
            .bool => |b| b,
        };
    }

    pub fn equal(self: Value, other: Value) bool {
        if (@enumToInt(self) != @enumToInt(other)) return false;

        return switch (self) {
            .nil => true,
            .number => |n| n == other.number,
            .bool => |b| b == other.bool,
        };
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .nil => try writer.writeAll("nil"),
            .number => |n| try writer.print("{}", .{n}),
            .bool => |b| try writer.print("{}", .{b}),
        }
    }
};
