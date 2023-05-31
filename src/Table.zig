const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("Object.zig");
const Value = @import("value.zig").Value;

const Table = @This();

allocator: Allocator,
entries: []Entry,
size: usize,
available: usize,

const minimum_capacity = 8;
const max_load_percentage = 75;

const Entry = struct {
    key: ?*Object.String = null,
    value: Value = .nil,

    fn tombstone(self: *Entry) void {
        self.* = .{
            .key = null,
            .value = .{ .bool = true },
        };
    }

    fn isTombstone(self: Entry) bool {
        return self.key == null and self.value.eql(.{ .bool = true });
    }
};

const GetResult = struct {
    key_ptr: **Object.String,
    value_ptr: *Value,
};

const GetOrPutResult = struct {
    key_ptr: **Object.String,
    value_ptr: *Value,
    found_existing: bool,
};

pub fn init(allocator: Allocator) Table {
    return .{
        .allocator = allocator,
        .entries = &[_]Entry{},
        .size = 0,
        .available = 0,
    };
}

pub fn initWithCapacity(allocator: Allocator, cap: usize) Allocator.Error!Table {
    const actual_cap = @max(cap, minimum_capacity);
    const entries = try allocator.alloc(Entry, actual_cap);
    initEntries(entries);

    return .{
        .allocator = allocator,
        .entries = entries,
        .size = 0,
        .available = actual_cap * max_load_percentage / 100,
    };
}

fn initEntries(entries: []Entry) void {
    for (entries) |*entry| {
        entry.* = Entry{};
    }
}

pub fn deinit(self: *Table) void {
    self.allocator.free(self.entries);
    self.* = undefined;
}

pub fn put(self: *Table, key: *Object.String, value: Value) Allocator.Error!bool {
    const gop = try self.getOrPut(key);
    gop.value_ptr.* = value;

    return !gop.found_existing;
}

fn putAssumeCapacityNoClobber(self: *Table, key: *Object.String, value: Value) void {
    const entry = self.findEntry(key).?;
    entry.* = .{
        .key = key,
        .value = value,
    };
    self.size += 1;

    std.debug.assert(self.available > 0);
    self.available -= 1;
}

pub fn getOrPut(self: *Table, key: *Object.String) Allocator.Error!GetOrPutResult {
    self.growIfNeeded(1) catch |err| {
        const entry = self.findEntry(key) orelse return err;
        if (entry.key) |*k| {
            return .{
                .key_ptr = k,
                .value_ptr = &entry.value,
                .found_existing = true,
            };
        }
        return err;
    };

    var found_existing = true;

    const entry = self.findEntry(key).?;
    if (entry.key == null) {
        if (!entry.isTombstone()) {
            std.debug.assert(self.available > 0);
            self.available -= 1;
        }

        entry.* = .{
            .key = key,
            .value = undefined,
        };
        self.size += 1;

        found_existing = false;
    }

    return .{
        .key_ptr = &entry.key.?,
        .value_ptr = &entry.value,
        .found_existing = found_existing,
    };
}

pub fn get(self: *Table, key: anytype) ?Value {
    const entry = self.getEntry(key) orelse return null;
    return entry.value_ptr.*;
}

pub fn getKey(self: *Table, key: anytype) ?*Object.String {
    const entry = self.getEntry(key) orelse return null;
    return entry.key_ptr.*;
}

pub fn getEntry(self: *Table, key: anytype) ?GetResult {
    const entry = self.findEntry(key) orelse return null;
    if (entry.key) |*key_ptr| {
        return .{
            .key_ptr = key_ptr,
            .value_ptr = &entry.value,
        };
    } else {
        return null;
    }
}

fn findEntry(self: *Table, key: anytype) ?*Entry {
    if (self.available == 0) return null;

    var index = hash(key) % self.capacity();
    var tombstone: ?*Entry = null;
    while (true) {
        const entry = &self.entries[index];
        defer index = (index + 1) % self.capacity();

        if (entry.isTombstone()) {
            if (tombstone == null) tombstone = entry;
            continue;
        }

        if (entry.key) |k| {
            if (k.eql(key)) return entry;
        } else {
            if (tombstone) |t| {
                return t;
            } else {
                return entry;
            }
        }
    }
}

pub fn remove(self: *Table, key: anytype) bool {
    if (self.size == 0) return false;

    const entry = self.getEntry(key) orelse return false;
    self.size -= 1;
    entry.tombstone();

    return true;
}

pub fn capacity(self: Table) usize {
    return self.entries.len;
}

pub fn extend(self: *Table, other: Table) Allocator.Error!void {
    for (other.entries) |entry| {
        if (entry.key) |key| {
            try self.put(key, entry.value);
        }
    }
}

fn growIfNeeded(self: *Table, added: usize) Allocator.Error!void {
    if (added > self.available) {
        try self.grow();
    }
}

fn grow(self: *Table) Allocator.Error!void {
    const new_capacity = self.capacity() * 2;
    var new_table = try Table.initWithCapacity(self.allocator, new_capacity);
    defer new_table.deinit();

    for (self.entries) |entry| {
        if (entry.key) |key| {
            new_table.putAssumeCapacityNoClobber(key, entry.value);
        }
    }

    std.mem.swap(Table, self, &new_table);
}

pub fn hash(value: anytype) u32 {
    switch (@TypeOf(value)) {
        []u8, []const u8 => {
            var h: u32 = 2166136261;
            for (value) |ch| {
                h ^= ch;
                h *%= 16777619;
            }
            return h;
        },

        *Object.String, *const Object.String => return value.hash,

        else => {
            @compileError("Cannot hash " ++ @typeName(@TypeOf(value)));
        },
    }
}
