const Parser = @import("regex_lib");
const std = @import("std");
const characterParser = Parser.characterParser;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const first = try characterParser().parse(allocator, "\\%");
    switch (first.value) {
        .ok => |val| std.debug.print("Value: {?}\nIndex: {d}\n", .{ val, first.index }),
        .err => std.debug.print("Error while parsing\n", .{}),
    }
}
