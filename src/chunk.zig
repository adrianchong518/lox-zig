const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Value = @import("value.zig").Value;

pub const OpCodeTag = enum(u8) {
    call,
    invoke,
    @"return",

    constant,
    closure,

    define_global,
    get_global,
    set_global,

    get_local,
    set_local,

    get_upvalue,
    set_upvalue,

    get_property,
    set_property,

    class,
    method,
    inherit,
    get_super,
    super_invoke,

    pop,
    close_upvalue,

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
    loop,

    _,
};

pub const OpCode = union(OpCodeTag) {
    call: struct { arg_count: u8 },
    invoke: struct { arg_count: u8, index: usize },
    @"return",

    constant: struct { index: usize },
    closure: struct { index: usize },

    define_global: struct { index: usize },
    get_global: struct { index: usize },
    set_global: struct { index: usize },

    get_local: struct { index: usize },
    set_local: struct { index: usize },

    get_upvalue: struct { index: usize },
    set_upvalue: struct { index: usize },

    get_property: struct { index: usize },
    set_property: struct { index: usize },

    class: struct { index: usize },
    method: struct { index: usize },
    inherit,
    get_super: struct { index: usize },
    super_invoke: struct { arg_count: u8, index: usize },

    pop,
    close_upvalue,

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
    loop: struct { offset: u16 = u16_placeholder },

    const u16_placeholder: u16 = 0xaaaa;

    pub const Upvalue = struct {
        index: usize,
        locality: Locality,

        pub const Locality = enum(u1) { local, non_local };

        pub fn eql(self: Upvalue, other: Upvalue) bool {
            return self.index == other.index and self.locality == other.locality;
        }
    };
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
            .call => .{ .call = .{ .arg_count = self.next(offset) } },
            .invoke => .{ .invoke = .{
                .arg_count = self.next(offset),
                .index = self.nextUsize(offset),
            } },
            .@"return" => .@"return",

            .constant => .{ .constant = .{ .index = self.nextUsize(offset) } },
            .closure => .{ .closure = .{ .index = self.nextUsize(offset) } },

            .define_global => .{ .define_global = .{ .index = self.nextUsize(offset) } },
            .get_global => .{ .get_global = .{ .index = self.nextUsize(offset) } },
            .set_global => .{ .set_global = .{ .index = self.nextUsize(offset) } },

            .get_local => .{ .get_local = .{ .index = self.nextUsize(offset) } },
            .set_local => .{ .set_local = .{ .index = self.nextUsize(offset) } },

            .get_upvalue => .{ .get_upvalue = .{ .index = self.nextUsize(offset) } },
            .set_upvalue => .{ .set_upvalue = .{ .index = self.nextUsize(offset) } },

            .get_property => .{ .get_property = .{ .index = self.nextUsize(offset) } },
            .set_property => .{ .set_property = .{ .index = self.nextUsize(offset) } },

            .class => .{ .class = .{ .index = self.nextUsize(offset) } },
            .method => .{ .method = .{ .index = self.nextUsize(offset) } },
            .inherit => .inherit,
            .get_super => .{ .get_super = .{ .index = self.nextUsize(offset) } },
            .super_invoke => .{ .super_invoke = .{
                .arg_count = self.next(offset),
                .index = self.nextUsize(offset),
            } },

            .pop => .pop,
            .close_upvalue => .close_upvalue,

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

            .jump => .{ .jump = .{ .offset = self.nextU16(offset) } },
            .jump_if_false => .{ .jump_if_false = .{ .offset = self.nextU16(offset) } },
            .loop => .{ .loop = .{ .offset = self.nextU16(offset) } },

            _ => null,
        };
    }

    fn nextUsize(self: Chunk, offset: *usize) usize {
        var constant_offset: usize = 0;

        while (self.read(offset.*) & 0b1000_0000 != 0) {
            const byte = self.next(offset);
            constant_offset |= byte & 0b0111_1111;
            constant_offset <<= 7;
        }

        constant_offset |= self.next(offset) & 0b0111_1111;

        return constant_offset;
    }

    fn nextU16(self: Chunk, offset: *usize) u16 {
        return @as(u16, self.next(offset)) << 8 | self.next(offset);
    }

    pub fn nextUpvalue(self: Chunk, offset: *usize) OpCode.Upvalue {
        const locality = @intToEnum(OpCode.Upvalue.Locality, self.next(offset));
        const index = self.nextUsize(offset);

        return .{
            .index = index,
            .locality = locality,
        };
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
            .call => |op| _ = try self.writeInt(op.arg_count, line),
            .invoke => |op| {
                _ = try self.writeInt(op.arg_count, line);
                _ = try self.writeInt(op.index, line);
            },

            .constant => |op| try self.writeInt(op.index, line),
            .closure => |op| try self.writeInt(op.index, line),

            .define_global => |op| try self.writeInt(op.index, line),
            .get_global => |op| try self.writeInt(op.index, line),
            .set_global => |op| try self.writeInt(op.index, line),

            .get_local => |op| try self.writeInt(op.index, line),
            .set_local => |op| try self.writeInt(op.index, line),

            .get_upvalue => |op| try self.writeInt(op.index, line),
            .set_upvalue => |op| try self.writeInt(op.index, line),

            .get_property => |op| try self.writeInt(op.index, line),
            .set_property => |op| try self.writeInt(op.index, line),

            .class => |op| try self.writeInt(op.index, line),
            .method => |op| try self.writeInt(op.index, line),
            .get_super => |op| try self.writeInt(op.index, line),
            .super_invoke => |op| {
                _ = try self.writeInt(op.arg_count, line);
                _ = try self.writeInt(op.index, line);
            },

            .jump => |op| try self.writeInt(op.offset, line),
            .jump_if_false => |op| try self.writeInt(op.offset, line),
            .loop => |op| try self.writeInt(op.offset, line),

            else => {},
        }

        return loc;
    }

    fn writeInt(self: *Chunk, int: anytype, line: usize) Allocator.Error!void {
        switch (@TypeOf(int)) {
            usize => try self.writeUsize(int, line),
            u16 => try self.writeU16(int, line),
            u8 => _ = try self.write(int, line),
            else => @compileError("Cannot write offset of type: " ++ @typeName(@TypeOf(int))),
        }
    }

    fn writeUsize(self: *Chunk, offset: usize, line: usize) Allocator.Error!void {
        if (offset > 0b0111_1111) {
            try self.writeLongUsize(offset >> 7, line);
        }

        _ = try self.write(@truncate(u8, offset & 0b0111_1111), line);
    }

    fn writeLongUsize(self: *Chunk, offset: usize, line: usize) Allocator.Error!void {
        if (offset > 0b0111_1111) {
            try self.writeLongUsize(offset >> 7, line);
        }
        _ = try self.write(@truncate(u8, (offset & 0b0111_1111) | 0b1000_0000), line);
    }

    fn writeU16(self: *Chunk, offset: u16, line: usize) Allocator.Error!void {
        _ = try self.write(@truncate(u8, offset >> 8), line);
        _ = try self.write(@truncate(u8, offset), line);
    }

    pub fn patchU16(self: *Chunk, offset: u16, loc: usize) void {
        self.code.items[loc] = @truncate(u8, offset >> 8);
        self.code.items[loc + 1] = @truncate(u8, offset);
    }

    pub fn writeUpvalues(
        self: *Chunk,
        upvalues: []const OpCode.Upvalue,
        line: usize,
    ) Allocator.Error!void {
        for (upvalues) |u| {
            _ = try self.write(@enumToInt(u.locality), line);
            _ = try self.writeUsize(u.index, line);
        }
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
