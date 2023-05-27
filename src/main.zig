const std = @import("std");

const debug = @import("debug.zig");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Vm = @import("vm.zig").Vm;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    try chunk.writeConstant(1.2, 123);
    try chunk.writeConstant(3.4, 123);
    try chunk.writeOpCode(.add, 123);

    try chunk.writeConstant(5.6, 123);
    try chunk.writeOpCode(.divide, 123);

    try chunk.writeOpCode(.negate, 123);
    try chunk.writeOpCode(.ret, 123);

    // debug.disassemble(&chunk, "test chunk");

    var vm = try Vm.init(allocator, &chunk);
    defer vm.deinit();

    try vm.run();
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
