const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Value = @import("value.zig").Value;

pub const OpCodeTag = enum(u8) {
    @"return",

    constant,
    define_global,
    get_global,
    set_global,
    get_local,
    set_local,

    pop,

    nil,
    true,
    false,

    equal,
    greater,
    less,

    not,
    negate,
    add,
    subtract,
    multiply,
    divide,

    print,

    jump,
    jump_if_false,

    _,
};

pub const OpCode = union(OpCodeTag) {
    @"return",

    constant: struct { offset: usize },
    define_global: struct { offset: usize },
    get_global: struct { offset: usize },
    set_global: struct { offset: usize },
    get_local: struct { offset: usize },
    set_local: struct { offset: usize },

    pop,

    nil,
    true,
    false,

    equal,
    greater,
    less,

    not,
    negate,
    add,
    subtract,
    multiply,
    divide,

    print,

    jump: struct { offset: u16 = u16_placeholder },
    jump_if_false: struct { offset: u16 = u16_placeholder },

    const u16_placeholder: u16 = 0xaaaa;
};

pub const Chunk = struct {
    code: ArrayList(u8),
    lines: ArrayList(LineStart),
    constants: ArrayList(Value),

    pub const constant_max_amount = 16_777_216;

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
        self.* = undefined;
    }

    pub fn read(self: Chunk, offset: usize) u8 {
        return self.code.items[offset];
    }

    fn next(self: Chunk, offset: *usize) u8 {
        const byte = self.read(offset.*);
        offset.* += 1;
        return byte;
    }

    /// Returns the corresponding `OpCode` at `code.items[offset]`, and mutates `offset` to be the
    /// start of the next instruction
    pub fn nextOpCode(self: Chunk, offset: *usize) ?OpCode {
        const instruction = self.next(offset);
        return switch (@intToEnum(OpCodeTag, instruction)) {
            .@"return" => .@"return",

            .constant => .{ .constant = .{ .offset = self.nextOffsetUsize(offset) } },
            .define_global => .{ .define_global = .{ .offset = self.nextOffsetUsize(offset) } },
            .get_global => .{ .get_global = .{ .offset = self.nextOffsetUsize(offset) } },
            .set_global => .{ .set_global = .{ .offset = self.nextOffsetUsize(offset) } },
            .get_local => .{ .get_local = .{ .offset = self.nextOffsetUsize(offset) } },
            .set_local => .{ .set_local = .{ .offset = self.nextOffsetUsize(offset) } },

            .pop => .pop,

            .nil => .nil,
            .true => .true,
            .false => .false,

            .equal => .equal,
            .greater => .greater,
            .less => .less,

            .not => .not,
            .negate => .negate,
            .add => .add,
            .subtract => .subtract,
            .multiply => .multiply,
            .divide => .divide,

            .print => .print,

            .jump => .{ .jump = .{ .offset = self.nextOffsetU16(offset) } },
            .jump_if_false => .{ .jump_if_false = .{ .offset = self.nextOffsetU16(offset) } },

            _ => null,
        };
    }

    fn nextOffsetUsize(self: Chunk, offset: *usize) usize {
        var constant_offset: usize = 0;

        while (self.read(offset.*) & 0b1000_0000 != 0) {
            const byte = self.next(offset);
            constant_offset |= byte & 0b0111_1111;
            constant_offset <<= 7;
        }

        constant_offset |= self.next(offset) & 0b0111_1111;

        return constant_offset;
    }

    fn nextOffsetU16(self: Chunk, offset: *usize) u16 {
        return @as(u16, self.next(offset)) << 8 | self.next(offset);
    }

    pub fn write(self: *Chunk, byte: u8, line: usize) Allocator.Error!usize {
        try self.code.append(byte);

        if (self.lines.getLastOrNull()) |line_start| {
            if (line_start.line == line) {
                return self.code.items.len - 1;
            }
        }

        try self.lines.append(.{ .offset = self.code.items.len - 1, .line = line });
        return self.code.items.len - 1;
    }

    pub fn writeOpCode(self: *Chunk, op_code: OpCode, line: usize) Allocator.Error!usize {
        const loc = try self.write(@enumToInt(op_code), line);

        switch (op_code) {
            .constant => |op| try self.writeOffset(op.offset, line),
            .define_global => |op| try self.writeOffset(op.offset, line),
            .get_global => |op| try self.writeOffset(op.offset, line),
            .set_global => |op| try self.writeOffset(op.offset, line),
            .get_local => |op| try self.writeOffset(op.offset, line),
            .set_local => |op| try self.writeOffset(op.offset, line),

            .jump => |op| try self.writeOffset(op.offset, line),
            .jump_if_false => |op| try self.writeOffset(op.offset, line),

            else => {},
        }

        return loc;
    }

    fn writeOffset(self: *Chunk, offset: anytype, line: usize) Allocator.Error!void {
        switch (@TypeOf(offset)) {
            usize => try self.writeOffsetUsize(offset, line),
            u16 => try self.writeOffsetU16(offset, line),
            else => @compileError("Cannot write offset of type: " ++ @typeName(@TypeOf(offset))),
        }
    }

    fn writeOffsetUsize(self: *Chunk, offset: usize, line: usize) Allocator.Error!void {
        if (offset > 0b0111_1111) {
            try self.writeLongOffsetUsize(offset >> 7, line);
        }

        _ = try self.write(@truncate(u8, offset & 0b0111_1111), line);
    }

    fn writeLongOffsetUsize(self: *Chunk, offset: usize, line: usize) Allocator.Error!void {
        if (offset > 0b0111_1111) {
            try self.writeLongOffsetUsize(offset >> 7, line);
        }
        _ = try self.write(@truncate(u8, (offset & 0b0111_1111) | 0b1000_0000), line);
    }

    fn writeOffsetU16(self: *Chunk, offset: u16, line: usize) Allocator.Error!void {
        _ = try self.write(@truncate(u8, offset >> 8), line);
        _ = try self.write(@truncate(u8, offset), line);
    }

    pub fn patchOffsetU16(self: *Chunk, offset: u16, loc: usize) void {
        self.code.items[loc] = @truncate(u8, offset >> 8);
        self.code.items[loc + 1] = @truncate(u8, offset);
    }

    pub fn addConstant(self: *Chunk, value: anytype) Allocator.Error!usize {
        try self.constants.append(Value.from(value));
        return self.constants.items.len - 1;
    }

    pub fn getLine(self: Chunk, offset: usize) usize {
        std.debug.assert(self.lines.items.len > 0);

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
