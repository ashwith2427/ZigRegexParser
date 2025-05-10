const std = @import("std");

pub fn Queue(comptime T: type) type {
    return struct {
        buffer: std.ArrayList(T),
        head: u32,
        tail: u32,
        allocator: std.mem.Allocator,
        const Self = @This();
        pub fn init(allocator: std.mem.Allocator) !Self {
            return .{
                .buffer = std.ArrayList(T).init(allocator),
                .head = 0,
                .tail = 0,
                .allocator = allocator,
            };
        }
        pub fn deinit(self: *Self) void {
            self.buffer.deinit();
        }
        pub fn push(self: *Self, item: T) !void {
            try self.buffer.insert(0, item);
            self.tail += 1;
        }
        pub fn pop(self: *Self) !T {
            const val = self.buffer.pop();
            if (val) |v| {
                return v;
            }
            return error.NoElements;
        }

        pub fn isempty(self: *Self) bool {
            return self.len() == 0;
        }

        pub fn len(self: *Self) usize {
            return self.buffer.items.len;
        }

        pub fn items(self: *Self) []T {
            return self.buffer;
        }

        pub fn get(self: *Self, idx: u32) T {
            return self.buffer[idx];
        }

        pub fn format(
            self: @This(),
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;
            for (self.buffer.items) |item| {
                try writer.print("{any} ", .{item});
            }
            try writer.print("\n", .{});
        }
    };
}
