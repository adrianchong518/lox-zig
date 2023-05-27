const std = @import("std");

const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;

pub fn compile(source: []const u8) !void {
    var scanner = Scanner.init(source);

    var line: usize = 0;
    while (true) {
        const token = scanner.scanToken();
        if (token.line != line) {
            line = token.line;
            std.debug.print("{: >4} | ", .{line});
        } else {
            std.debug.print("     | ", .{});
        }
        std.debug.print("{: >2} {s: <15} '{s}'\n", .{ @enumToInt(token.typ), @tagName(token.typ), token.lexeme });

        if (token.typ == .eof) {
            break;
        }
    }
}
