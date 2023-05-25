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

fn disassembleInstruction(chunk: *const Chunk, offset: usize) usize {
    std.debug.print("{:0>4} ", .{offset});

    const line = chunk.getLine(offset);
    if (offset > 0 and
        line == chunk.getLine(offset - 1))
    {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{: >4} ", .{line});
    }

    const instruction = chunk.code.items[offset];
    switch (@intToEnum(OpCodeLabel, instruction)) {
        .ret => return simpleInstruction("OP_RETURN", offset),
        .constant => return constantInstruction("OP_CONSTANT", chunk, offset),
        .constant_long => return longConstantInstruction("OP_CONSTANT_LONG", chunk, offset),
        _ => {
            std.debug.print("Unknown opcode {}\n", .{instruction});
            return offset + 1;
        },
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
    const constant_offset = chunk.code.items[offset + 1];
    printConstantInstruction(name, chunk, constant_offset);
    return offset + 2;
}

fn longConstantInstruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
    const constant_offset = chunk.code.items[offset + 1] |
        (@as(u24, chunk.code.items[offset + 2]) << 8) |
        (@as(u24, chunk.code.items[offset + 3]) << 16);

    printConstantInstruction(name, chunk, constant_offset);
    return offset + 4;
}

fn printConstantInstruction(
    name: []const u8,
    chunk: *const Chunk,
    constant_offset: usize,
) void {
    std.debug.print("{s: <16} {: >4} '", .{ name, constant_offset });
    printValue(chunk.constants.items[constant_offset]);
    std.debug.print("'\n", .{});
}

fn printValue(value: Value) void {
    std.debug.print("{}", .{value});
}
