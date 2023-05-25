const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Value = @import("value.zig").Value;

pub const OpCode = enum(u8) {
    ret,
    constant,
    _,
};

pub const Chunk = struct {
    code: ArrayList(u8),
    lines: ArrayList(LineStart),
    constants: ArrayList(Value),

    const LineStart = struct {
        offset: usize,
        line: usize,
    };

    pub fn init(allocator: Allocator) Chunk {
        return .{
            .code = ArrayList(u8).init(allocator),
            .lines = ArrayList(LineStart).init(allocator),
            .constants = ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    pub fn write(self: *Chunk, byte: u8, line: usize) Allocator.Error!void {
        try self.code.append(byte);

        if (self.lines.getLastOrNull()) |line_start| {
            if (line_start.line == line) {
                return;
            }
        }

        try self.lines.append(.{ .offset = self.code.items.len - 1, .line = line });
    }

    pub fn writeOpCode(self: *Chunk, op_code: OpCode, line: usize) Allocator.Error!void {
        try self.write(@enumToInt(op_code), line);
    }

    pub fn addConstant(self: *Chunk, value: Value) Allocator.Error!usize {
        try self.constants.append(value);
        return self.constants.items.len - 1;
    }

    pub fn getLine(self: *const Chunk, offset: usize) usize {
        var start: usize = 0;
        var end = self.lines.items.len - 1;

        while (true) {
            const mid = (start + end) / 2;
            const mid_line = &self.lines.items[mid];

            if (offset < mid_line.offset) {
                end = mid - 1;
            } else if (mid == self.lines.items.len - 1 or
                offset < self.lines.items[mid + 1].offset)
            {
                return mid_line.line;
            } else {
                start = mid + 1;
            }
        }
    }
};
