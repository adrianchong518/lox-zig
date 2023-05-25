const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
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

    if (offset > 0 and
        chunk.lines.items[offset] == chunk.lines.items[offset - 1])
    {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{: >4} ", .{chunk.lines.items[offset]});
    }

    const instruction = chunk.code.items[offset];
    switch (@intToEnum(OpCode, instruction)) {
        .ret => return simpleInstruction("OP_RETURN", offset),
        .constant => return constantInstruction("OP_CONSTANT", chunk, offset),
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

    std.debug.print("{s: <16} {: >4} `", .{ name, constant_offset });
    printValue(chunk.constants.items[constant_offset]);
    std.debug.print("`\n", .{});

    return offset + 2;
}

fn printValue(value: Value) void {
    std.debug.print("{}", .{value});
}
