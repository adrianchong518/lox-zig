const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const config = @import("config");

const Vm = @import("Vm.zig");
const Value = @import("value.zig").Value;
const Object = @import("Object.zig");
const Parser = @import("compiler.zig").Parser;
const Compiler = @import("compiler.zig").Compiler;

const GcAllocator = @This();

parent_allocator: Allocator,
vm: *Vm,
parser: ?*Parser = null,

gray_stack: ArrayList(*Object),
bytes_allocated: usize = 0,
next_gc: usize = 1024 * 1024,

pub const log = std.log.scoped(.gc_allocator);

const heap_grow_factor = 2;

pub fn init(parent_allocator: Allocator, vm: *Vm) GcAllocator {
    return .{
        .parent_allocator = parent_allocator,
        .vm = vm,
        .gray_stack = ArrayList(*Object).init(parent_allocator),
    };
}

pub fn deinit(self: *GcAllocator) void {
    self.gray_stack.deinit();
}

pub fn allocator(self: *GcAllocator) Allocator {
    return .{
        .ptr = self,
        .vtable = &.{
            .alloc = alloc,
            .resize = resize,
            .free = free,
        },
    };
}

pub fn attach_parser(self: *GcAllocator, parser: *Parser) void {
    self.parser = parser;
}

pub fn detach_parser(self: *GcAllocator) void {
    self.parser = null;
}

fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
    const self = @ptrCast(*GcAllocator, @alignCast(@alignOf(GcAllocator), ctx));

    if (config.log_gc) log.debug("alloc {}", .{len});
    self.bytes_allocated += len;

    if (config.stress_gc or self.bytes_allocated > self.next_gc) {
        self.collectGarbage() catch return null;
    }

    const res = self.parent_allocator.rawAlloc(len, ptr_align, ret_addr);
    if (config.log_gc) log.debug("alloced {*} {}", .{ res, len });

    return res;
}

fn free(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, ret_addr: usize) void {
    const self = @ptrCast(*GcAllocator, @alignCast(@alignOf(GcAllocator), ctx));

    if (config.log_gc) log.debug("free {*} {}", .{ buf.ptr, buf.len });
    self.bytes_allocated -= buf.len;

    return self.parent_allocator.rawFree(buf, log2_buf_align, ret_addr);
}

fn resize(
    ctx: *anyopaque,
    buf: []u8,
    log2_buf_align: u8,
    new_len: usize,
    ret_addr: usize,
) bool {
    const self = @ptrCast(*GcAllocator, @alignCast(@alignOf(GcAllocator), ctx));

    if (config.log_gc) log.debug("resize {*} {} -> {}", .{ buf.ptr, buf.len, new_len });
    self.bytes_allocated = self.bytes_allocated - buf.len + new_len;

    if (new_len > buf.len) {
        if (config.stress_gc or self.bytes_allocated > self.next_gc) {
            self.collectGarbage() catch return false;
        }
    }

    return self.parent_allocator.rawResize(buf, log2_buf_align, new_len, ret_addr);
}

fn collectGarbage(self: *GcAllocator) Allocator.Error!void {
    if (config.log_gc) {
        log.info("------ begin ------", .{});

        var object = self.vm.objects;
        while (object) |o| : (object = o.next) {
            std.debug.print("{*} -> ", .{o});
        }
        std.debug.print("(end)\n", .{});
    }

    const before = self.bytes_allocated;

    try self.markRoots();
    try self.traceReferences();
    self.sweep();

    const after = self.bytes_allocated;
    self.next_gc = after * heap_grow_factor;

    if (config.log_gc) {
        var object = self.vm.objects;
        while (object) |o| : (object = o.next) {
            std.debug.print("{*} -> ", .{o});
        }
        std.debug.print("(end)\n", .{});

        {
            const s = std.fmt.fmtIntSizeBin;
            log.info(
                "collected {} ({} -> {}), next at {}",
                .{ s(before - after), s(before), s(after), s(self.next_gc) },
            );
        }
        log.info("------  end  ------", .{});
    }
}

fn markRoots(self: *GcAllocator) Allocator.Error!void {
    for (self.vm.stack.items()) |v| {
        try self.markValue(v);
    }

    try self.markValueTable(&self.vm.globals);

    for (self.vm.frames.items()) |f| {
        try self.markObject(&f.closure.object);
    }

    var upvalue = self.vm.open_upvalues;
    while (upvalue) |u| : (upvalue = u.next) {
        try self.markObject(&u.object);
    }

    if (self.parser) |p| {
        var compiler: ?*Compiler = p.current_compiler;
        while (compiler) |c| : (compiler = c.enclosing) {
            try self.markObject(&c.function.object);

            var constant_strings_keys = c.constant_strings.keyIterator();
            while (constant_strings_keys.next()) |k| {
                try self.markObject(&k.*.object);
            }
        }
    }
}

fn markValue(self: *GcAllocator, value: Value) Allocator.Error!void {
    switch (value) {
        .object => |obj| try self.markObject(obj),
        else => {},
    }
}

fn markValues(self: *GcAllocator, values: []Value) Allocator.Error!void {
    for (values) |v| {
        try self.markValue(v);
    }
}

fn markValueTable(self: *GcAllocator, table: *Object.String.HashMap(Value)) Allocator.Error!void {
    var entries = table.iterator();
    while (entries.next()) |e| {
        try self.markObject(&e.key_ptr.*.object);
        try self.markValue(e.value_ptr.*);
    }
}

fn markObject(self: *GcAllocator, object: *Object) Allocator.Error!void {
    if (object.is_marked) {
        if (config.log_gc) log.debug("marked {#}", .{Value.from(object)});
        return;
    }

    if (config.log_gc) log.debug("mark {#}", .{Value.from(object)});

    object.is_marked = true;
    try self.gray_stack.append(object);
}

fn traceReferences(self: *GcAllocator) Allocator.Error!void {
    while (self.gray_stack.popOrNull()) |obj| {
        if (config.log_gc) log.debug("blacken {#}", .{Value.from(obj)});

        switch (obj.typ) {
            .upvalue => try self.markValue(obj.as(.upvalue).closed),
            .function => {
                const function = obj.as(.function);
                if (function.name) |n| try self.markObject(&n.object);
                try self.markValues(function.chunk.constants.items);
            },
            .closure => {
                const closure = obj.as(.closure);
                try self.markObject(&closure.function.object);
                for (closure.upvalues[0..closure.upvalue_count]) |u| {
                    try self.markObject(&u.object);
                }
            },
            .class => {
                const class = obj.as(.class);
                try self.markObject(&class.name.object);
            },
            .instance => {
                const instance = obj.as(.instance);
                try self.markObject(&instance.class.object);
                try self.markValueTable(&instance.fields);
            },
            .native, .string => {},
        }
    }
}

fn sweep(self: *GcAllocator) void {
    {
        var strings_keys = self.vm.strings.keyIterator();
        while (strings_keys.next()) |k| {
            if (!k.*.object.is_marked) {
                self.vm.strings.removeByPtr(k);
            }
        }
    }

    {
        var previous: ?*Object = null;
        var object = self.vm.objects;

        while (object) |o| {
            if (o.is_marked) {
                o.is_marked = false;
                previous = o;
                object = o.next;
            } else {
                const unreached = o;
                object = o.next;
                if (previous) |p| {
                    p.next = object;
                } else {
                    self.vm.objects = object;
                }

                unreached.destroy(self.vm);
            }
        }
    }
}
