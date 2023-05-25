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

    for (0..0x101) |i| {
        try chunk.writeConstant(@intToFloat(f64, i), i);
    }

    try chunk.writeOpCode(.ret, 2000);

    debug.disassemble(&chunk, "test chunk");
}
