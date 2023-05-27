const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const OpCodeLabel = @import("chunk.zig").OpCodeLabel;
const Value = @import("value.zig").Value;

pub fn disassemble(chunk: *const Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: *const Chunk, offset: usize) usize {
    std.debug.print("{:0>4} ", .{offset});

    const line = chunk.getLine(offset);
    if (offset > 0 and
        line == chunk.getLine(offset - 1))
    {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{: >4} ", .{line});
    }

    var new_offset = offset;
    const instruction = chunk.nextOpCode(&new_offset);
    if (instruction) |inst| {
        switch (inst) {
            .ret => printSimple("OP_RETURN"),

            .constant => |op| printConstant("OP_CONSTANT", chunk, op.offset),
            .constant_long => |op| printConstant("OP_CONSTANT_LONG", chunk, op.offset),

            .negate => printSimple("OP_NEGATE"),
            .add => printSimple("OP_ADD"),
            .subtract => printSimple("OP_SUBTRACT"),
            .multiply => printSimple("OP_MULTIPLY"),
            .divide => printSimple("OP_DIVIDE"),
        }
    } else {
        std.debug.print("Unknown opcode: {0} (0x{0x})\n", .{chunk.read(offset)});
    }

    return new_offset;
}

fn printSimple(name: []const u8) void {
    std.debug.print("{s}\n", .{name});
}

fn printConstant(
    name: []const u8,
    chunk: *const Chunk,
    constant_offset: usize,
) void {
    std.debug.print("{s: <16} {: >4} '{}'\n", .{
        name,
        constant_offset,
        chunk.constants.items[constant_offset],
    });
}
