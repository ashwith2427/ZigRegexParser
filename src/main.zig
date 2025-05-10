const Parser = @import("regex_lib");
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var parser = try Parser.init("(a|b)*", allocator);
    defer parser.deinit();
    try parser.parse();
    try parser.print();
}
