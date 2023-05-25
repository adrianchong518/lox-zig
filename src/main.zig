const std = @import("std");

const debug = @import("debug.zig");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    const constant_offset = try chunk.addConstant(1.2);
    try chunk.writeOpCode(.constant, 123);
    try chunk.write(@intCast(u8, constant_offset), 123);

    try chunk.writeOpCode(.ret, 123);

    debug.disassemble(&chunk, "test chunk");
}
