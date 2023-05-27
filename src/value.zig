const std = @import("std");

pub const Value = struct {
    value: f64,

    pub fn from(value: anytype) Value {
        const T = @TypeOf(value);

        if (T == f64 or T == comptime_float) {
            return .{ .value = value };
        } else {
            @compileError("Unsupported value type: " ++ @typeName(T));
        }
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{}", .{self.value});
    }
};
