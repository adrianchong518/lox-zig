const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Value = @import("value.zig").Value;

pub const OpCodeLabel = enum(u8) {
    @"return",

    constant,
    constant_long,

    negate,
    add,
    subtract,
    multiply,
    divide,
    _,
};

pub const OpCode = union(OpCodeLabel) {
    @"return",

    constant: struct { offset: u8 },
    constant_long: struct { offset: u24 },

    negate,
    add,
    subtract,
    multiply,
    divide,
};

pub const Chunk = struct {
    const LineStart = struct {
        offset: usize,
        line: usize,
    };

    code: ArrayList(u8),
    lines: ArrayList(LineStart),
    constants: ArrayList(Value),

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

    pub fn read(self: *const Chunk, offset: usize) u8 {
        return self.code.items[offset];
    }

    fn next(self: *const Chunk, offset: *usize) u8 {
        const byte = self.read(offset.*);
        offset.* += 1;
        return byte;
    }

    /// Returns the corresponding `OpCode` at `code.items[offset]`, and mutates `offset` to be the
    /// start of the next instruction
    pub fn nextOpCode(self: *const Chunk, offset: *usize) ?OpCode {
        const instruction = self.next(offset);
        switch (@intToEnum(OpCodeLabel, instruction)) {
            .@"return" => return .@"return",

            .constant => return .{ .constant = .{ .offset = self.next(offset) } },
            .constant_long => return .{ .constant_long = .{
                .offset = self.next(offset) |
                    (@as(u24, self.next(offset)) << 8) |
                    (@as(u24, self.next(offset)) << 16),
            } },

            .negate => return .negate,

            .add => return .add,
            .subtract => return .subtract,
            .multiply => return .multiply,
            .divide => return .divide,

            _ => return null,
        }
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

        switch (op_code) {
            .constant => |op| try self.write(op.offset, line),

            .constant_long => |op| {
                try self.write(@truncate(u8, op.offset), line);
                try self.write(@truncate(u8, op.offset >> 8), line);
                try self.write(@truncate(u8, op.offset >> 16), line);
            },

            .@"return", .negate, .add, .subtract, .multiply, .divide => {},
        }
    }

    pub fn writeConstant(self: *Chunk, value: anytype, line: usize) Allocator.Error!void {
        const offset = try self.addConstant(value);

        if (offset <= 0xff) {
            try self.writeOpCode(.{
                .constant = .{ .offset = @intCast(u8, offset) },
            }, line);
        } else if (offset <= 0xff_ff_ff) {
            try self.writeOpCode(.{
                .constant_long = .{ .offset = @intCast(u24, offset) },
            }, line);
        } else {
            std.debug.panic(
                "there are more than 2^24 constants, which is not supported",
                .{},
            );
        }
    }

    fn addConstant(self: *Chunk, value: anytype) Allocator.Error!usize {
        try self.constants.append(Value.from(value));
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