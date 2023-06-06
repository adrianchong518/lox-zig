const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn FixedCapacityStack(comptime T: type) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        buffer: []T,
        top: usize,

        pub const Error = error{OutOfBounds};

        pub fn init(allocator: Allocator, capacity: usize) Allocator.Error!Self {
            const buffer = try allocator.alloc(T, capacity);
            return .{
                .allocator = allocator,
                .buffer = buffer,
                .top = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.buffer);
            self.* = undefined;
        }

        pub fn push(self: *Self, value: T) void {
            self.buffer[self.top] = value;
            self.top += 1;
        }

        pub fn tryPush(self: *Self, value: T) Error!void {
            if (self.top == self.buffer.len) return error.OutOfBounds;
            self.push(value);
        }

        pub fn pop(self: *Self) T {
            self.top -= 1;
            return self.buffer[self.top];
        }

        pub fn tryPop(self: *Self) ?T {
            if (self.top <= 0) return null;

            self.top -= 1;
            return self.buffer[self.top];
        }

        pub fn discardUntil(self: *Self, new_top: usize) void {
            std.debug.assert(new_top <= self.top);
            self.top = new_top;
        }

        pub fn peekIndex(self: Self, from_top: usize) usize {
            std.debug.assert(from_top < self.top);
            return self.top - from_top - 1;
        }

        pub fn peek(self: Self, from_top: usize) T {
            return self.peekPtr(from_top).*;
        }

        pub fn peekPtr(self: Self, from_top: usize) *T {
            return &self.buffer[self.peekIndex(from_top)];
        }

        pub fn items(self: *Self) []T {
            return self.buffer[0..self.top];
        }

        pub fn clear(self: *Self) void {
            self.top = 0;
        }

        pub fn isEmpty(self: Self) bool {
            return self.top == 0;
        }

        pub fn count(self: Self) usize {
            return self.top;
        }
    };
}

test "stack can push and pop" {
    var stack = try FixedCapacityStack(u8).init(std.testing.allocator, 256);
    defer stack.deinit();

    stack.push(1);
    stack.push(2);
    stack.push(3);
    stack.push(4);

    try std.testing.expectEqualSlices(u8, &[_]u8{ 1, 2, 3, 4 }, stack.items());

    try std.testing.expect(4 == stack.pop());
    try std.testing.expect(3 == stack.pop());
    try std.testing.expect(2 == stack.pop());
    try std.testing.expect(1 == stack.pop());
}
