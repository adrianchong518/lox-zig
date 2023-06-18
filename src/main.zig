const std = @import("std");
const fs = std.fs;
const io = std.io;
const process = std.process;
const Allocator = std.mem.Allocator;

const debug = @import("debug.zig");
const compiler = @import("compiler.zig");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Vm = @import("Vm.zig");

pub const InterpretError = error{
    CompileFailed,
    RuntimePanic,
} || fs.File.WriteError || Allocator.Error;

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
}

fn repl(allocator: Allocator) InterpretError!void {
    const stderr = io.getStdErr().writer();
    const stdin = io.getStdIn().reader();

    var vm = Vm.create();
    try vm.init(allocator);
    defer vm.deinit();

    var buffer: [256]u8 = undefined;
    while (true) {
        try stderr.writeAll("> ");
        const source = stdin.readUntilDelimiterOrEof(&buffer, '\n') catch {
            try stderr.writeAll("input too long (256 characters max)");
            continue;
        } orelse return;

        interpret(source, &vm) catch {};
    }
}

fn runFile(allocator: Allocator, file_path: []const u8) InterpretError!void {
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
            else => |e| try stderr.print("could not open file \"{s}\": {}", .{ file_path, e }),
        }
        process.exit(74);
    };
    defer allocator.free(source);

    var vm = Vm.create();
    try vm.init(allocator);
    defer vm.deinit();

    interpret(source, &vm) catch |err| switch (err) {
        error.CompileFailed => process.exit(65),
        error.RuntimePanic => process.exit(70),
        else => |e| return e,
    };
}

fn interpret(source: []const u8, vm: *Vm) InterpretError!void {
    const function = try compiler.compile(source, vm);
    try vm.interpret(function);
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
