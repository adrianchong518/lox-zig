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
    lines: ArrayList(usize),
    constants: ArrayList(Value),

    pub fn init(allocator: Allocator) Chunk {
        return .{
            .code = ArrayList(u8).init(allocator),
            .lines = ArrayList(usize).init(allocator),
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
        try self.lines.append(line);
    }

    pub fn writeOpCode(self: *Chunk, op_code: OpCode, line: usize) Allocator.Error!void {
        try self.write(@enumToInt(op_code), line);
    }

    pub fn addConstant(self: *Chunk, value: Value) Allocator.Error!usize {
        try self.constants.append(value);
        return self.constants.items.len - 1;
    }
};
