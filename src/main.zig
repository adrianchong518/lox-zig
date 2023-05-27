const std = @import("std");
const fs = std.fs;
const io = std.io;
const process = std.process;
const Allocator = std.mem.Allocator;

const debug = @import("debug.zig");
const compiler = @import("compiler.zig");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Vm = @import("vm.zig").Vm;

const InterpretError = error{
    Compile,
    Runtime,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    switch (args.len) {
        1 => try repl(allocator),
        2 => try runFile(allocator, args[1]),
        else => {
            const stderr = io.getStdErr().writer();
            try stderr.print("Usage: lox-zig [path]\n", .{});
        },
    }

    // var vm = try Vm.init(allocator, &chunk);
    // defer vm.deinit();

    // try vm.run();
}

fn repl(allocator: Allocator) !void {
    const stderr = io.getStdErr().writer();
    const stdin = io.getStdIn().reader();

    var vm = try Vm.init(allocator, io.getStdOut().writer(), stderr);
    defer vm.deinit();

    var buffer: [256]u8 = undefined;
    while (true) {
        try stderr.writeAll("> ");
        const source = stdin.readUntilDelimiterOrEof(&buffer, '\n') catch {
            try stderr.writeAll("input too long (256 characters max)");
            continue;
        } orelse return;

        interpret(source) catch {};
    }
}

fn runFile(allocator: Allocator, file_path: []const u8) !void {
    const stderr = io.getStdErr().writer();

    const source = fs.cwd().readFileAlloc(allocator, file_path, 1_048_576) catch |err| {
        switch (err) {
            error.FileNotFound => try stderr.print("file not found: {s}", .{file_path}),
            error.FileTooBig => {
                try stderr.print("file exceeds file size limit of 1MiB: {s}", .{file_path});
            },
            error.OutOfMemory => {
                try stderr.print("not enough memory to read file: {s}", .{file_path});
            },
            else => try stderr.print("could not open file \"{s}\": {}", .{ file_path, err }),
        }
        process.exit(74);
    };
    defer allocator.free(source);

    var vm = try Vm.init(allocator, io.getStdOut().writer(), io.getStdErr().writer());
    defer vm.deinit();

    interpret(source) catch |err| switch (err) {
        error.Compile => process.exit(65),
        error.Runtime => process.exit(70),
    };
}

fn interpret(source: []const u8) InterpretError!void {
    try compiler.compile(source);
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
