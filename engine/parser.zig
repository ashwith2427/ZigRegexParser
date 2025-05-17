const std = @import("std");
const Lexer = @import("lexer.zig");
const Token = Lexer.Token;
const TokenKind = Lexer.TokenKind;
const Allocator = std.mem.Allocator;

const ParseError = error{
    UnexpectedEOF,
};

const Diagnostics = struct {
    token: ?Token = null,
    type: []const u8 = null,
    message: []const u8,
    context: ?[]const u8 = null,
    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("Error: {}\n", .{self.type});
        if (self.token) |tok| {
            try writer.print("\tPosition: {}\n", .{tok.position});
        }
        try writer.print("\tMessage: {}\n", .{self.message});
    }
};

const TokenIterator = struct {
    const Self = @This();
    index: usize,
    elements: []Token,
    allocator: std.mem.Allocator,

    pub fn init(elements: []Token, allocator: std.mem.Allocator) Self {
        return .{
            .index = 0,
            .elements = elements,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.elements);
    }

    pub fn hasNext(self: *Self) bool {
        return self.index < self.elements.len;
    }

    pub fn next(self: *Self) ?Token {
        if (!self.hasNext()) return null;
        defer self.index += 1;
        return self.elements[self.index];
    }

    pub fn peek(self: *Self) ?Token {
        return if (self.hasNext()) self.elements[self.index] else null;
    }

    pub fn expect(self: *Self, tag: TokenKind) ParseError!Token {
        const tok = self.peek() orelse return ParseError.UnexpectedEOF;
        if (tok.kind != tag) {
            return ParseError.UnexpectedToken;
        }
        return self.next() orelse unreachable;
    }
};

iterator: TokenIterator,

pub fn Result(comptime T: type) type {
    return struct {
        index: u32,
        value: union(enum) {
            ok: T,
            err: ParseError,
        },
        pub fn ok(index: u32, value: T) Result(T) {
            return .{ .index = index, .value = value };
        }
        pub fn err(index: u32) Result(T) {
            return .{ .index = index, .value = {} };
        }
    };
}

pub fn Parser(comptime Value: type) type {
    return struct {
        pub const T = Value;
        const Self = @This();
        _parse: *const fn (*Allocator, []const u8) callconv(.Inline) ParseError!?Value,
    };
}

pub fn stringParser(comptime str: []const u8) Parser([]const u8) {
    return .{
        .parse = struct {
            pub fn parse(_: *Allocator, s: []const u8) ParseError!Result([]const u8) {
                if (!std.mem.startsWith(u8, s, str)) {
                    return Result([]const u8).err(0);
                }
                return Result([]const u8).ok(str.len, str);
            }
        }.parse,
    };
}

pub fn ParserResult(comptime P: type) type {
    return switch (@typeInfo(P)) {
        .pointer => |p| p.child.T,
        else => P.T,
    };
}

pub fn seq(comptime parsers: anytype) Parser(ParserResult(@TypeOf(parsers[0]))) {}
