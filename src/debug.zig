const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const OpCodeLabel = @import("chunk.zig").OpCodeLabel;
const Value = @import("value.zig").Value;

pub fn disassemble(chunk: Chunk, name: anytype) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: Chunk, offset: usize) usize {
    std.debug.print("{:0>4} ", .{offset});

    const line = chunk.getLine(offset);
    if (offset > 0 and
        line == chunk.getLine(offset - 1))
    {
        std.debug.print("     | ", .{});
    } else {
        std.debug.print("{: >4} | ", .{line});
    }

    var new_offset = offset;
    const instruction = chunk.nextOpCode(&new_offset) orelse {
        std.debug.print("Unknown opcode: {0} (0x{0x})\n", .{chunk.read(offset)});
        return new_offset;
    };

    std.debug.print("{x:0>2} ", .{@enumToInt(instruction)});

    switch (instruction) {
        .call => |op| printInt("OP_CALL", op.arg_count),
        .invoke => |op| printInvoke("OP_INVOKE", chunk, op.arg_count, op.index),
        .@"return" => printSimple("OP_RETURN"),

        .constant => |op| printConstant("OP_CONSTANT", chunk, op.index),
        .closure => |op| printClosure("OP_CLOSURE", chunk, op.index, &new_offset),

        .define_global => |op| printConstant("OP_DEFINE_GLOBAL", chunk, op.index),
        .get_global => |op| printConstant("OP_GET_GLOBAL", chunk, op.index),
        .set_global => |op| printConstant("OP_SET_GLOBAL", chunk, op.index),

        .get_local => |op| printInt("OP_GET_LOCAL", op.index),
        .set_local => |op| printInt("OP_SET_LOCAL", op.index),

        .get_upvalue => |op| printInt("OP_GET_UPVALUE", op.index),
        .set_upvalue => |op| printInt("OP_SET_UPVALUE", op.index),

        .get_property => |op| printConstant("OP_GET_PROPERTY", chunk, op.index),
        .set_property => |op| printConstant("OP_SET_PROPERTY", chunk, op.index),

        .class => |op| printConstant("OP_CLASS", chunk, op.index),
        .method => |op| printConstant("OP_METHOD", chunk, op.index),
        .inherit => printSimple("OP_INHERIT"),
        .get_super => |op| printConstant("OP_GET_SUPER", chunk, op.index),
        .super_invoke => |op| printInvoke("OP_SUPER_INVOKE", chunk, op.arg_count, op.index),

        .pop => printSimple("OP_POP"),
        .close_upvalue => printSimple("OP_CLOSE_UPVALUE"),

        .nil => printSimple("OP_NIL"),
        .true => printSimple("OP_TRUE"),
        .false => printSimple("OP_FALSE"),

        .equal => printSimple("OP_EQUAL"),
        .greater => printSimple("OP_GREATER"),
        .less => printSimple("OP_LESS"),

        .not => printSimple("OP_NOT"),
        .negate => printSimple("OP_NEGATE"),
        .add => printSimple("OP_ADD"),
        .subtract => printSimple("OP_SUBTRACT"),
        .multiply => printSimple("OP_MULTIPLY"),
        .divide => printSimple("OP_DIVIDE"),

        .print => printSimple("OP_PRINT"),

        .jump => |op| printJump("OP_JUMP", offset, .forward, op.offset),
        .jump_if_false => |op| printJump("OP_JUMP_IF_FALSE", offset, .forward, op.offset),
        .loop => |op| printJump("OP_LOOP", offset, .backward, op.offset),
    }

    return new_offset;
}

fn printSimple(name: []const u8) void {
    std.debug.print("{s}\n", .{name});
}

fn printConstant(name: []const u8, chunk: Chunk, constant_index: usize) void {
    std.debug.print("{s: <16} {: >4} {#}\n", .{
        name,
        constant_index,
        chunk.constants.items[constant_index],
    });
}

fn printInt(name: []const u8, byte: anytype) void {
    std.debug.print("{s: <16} {: >4}\n", .{ name, byte });
}

fn printJump(
    name: []const u8,
    instruction_offset: usize,
    direction: enum { forward, backward },
    offset: u16,
) void {
    const target = switch (direction) {
        .forward => instruction_offset + 3 + offset,
        .backward => instruction_offset + 3 - offset,
    };
    std.debug.print("{s: <16} {: >4} -> {}\n", .{ name, instruction_offset, target });
}

fn printClosure(name: []const u8, chunk: Chunk, function_offset: usize, new_offset: *usize) void {
    const function = chunk.constants.items[function_offset].object.as(.function);
    std.debug.print("{s: <16} {: >4} {#}\n", .{ name, function_offset, function });

    for (0..function.upvalue_count) |_| {
        std.debug.print("{:0>4}      | ", .{new_offset.*});
        const upvalue = chunk.nextUpvalue(new_offset);

        std.debug.print(
            "                         {s: <10} {}\n",
            .{ @tagName(upvalue.locality), upvalue.index },
        );
    }
}

fn printInvoke(name: []const u8, chunk: Chunk, arg_count: u8, constant_index: usize) void {
    std.debug.print("{s: <16} ({: >4} args) {: >4} {#}\n", .{
        name,
        arg_count,
        constant_index,
        chunk.constants.items[constant_index],
    });
}
