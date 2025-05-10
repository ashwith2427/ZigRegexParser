const std = @import("std");

const Lexer = @This();

cursor: u32,
allocator: std.mem.Allocator,
pattern: []const u8,

pub const TokenKind = union(enum) {
    Plus,
    Star,
    Alter,
    Character: u8,
    LiteralCharacter: u8,
    Space,
    Tab,
    Word,
    Line,
    StartAnchor,
    EndAnchor,
    Dot,
    Question,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
};

pub const Token = struct {
    position: u32,
    kind: TokenKind,
    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print(
            "position: {d}, kind: {s}",
            .{ self.position, @tagName(self.kind) },
        );
    }
};

pub fn init(pattern: []const u8, allocator: std.mem.Allocator) Lexer {
    return .{
        .cursor = 0,
        .pattern = pattern,
        .allocator = allocator,
    };
}

pub fn scan(self: *Lexer) ![]Token {
    var tokens = std.ArrayList(Token).init(self.allocator);
    while (self.cursor < self.pattern.len) : (self.cursor += 1) {
        try switch (self.pattern[self.cursor]) {
            '*' => tokens.append(.{
                .position = self.cursor,
                .kind = .Star,
            }),
            '+' => tokens.append(.{
                .position = self.cursor,
                .kind = .Plus,
            }),
            '.' => tokens.append(.{
                .position = self.cursor,
                .kind = .Dot,
            }),
            '|' => tokens.append(.{
                .position = self.cursor,
                .kind = .Alter,
            }),
            '?' => tokens.append(.{
                .position = self.cursor,
                .kind = .Question,
            }),
            '^' => tokens.append(.{
                .position = self.cursor,
                .kind = .StartAnchor,
            }),
            '$' => tokens.append(.{
                .position = self.cursor,
                .kind = .EndAnchor,
            }),
            '(' => tokens.append(.{
                .position = self.cursor,
                .kind = .OpenParen,
            }),
            ')' => tokens.append(.{
                .position = self.cursor,
                .kind = .CloseParen,
            }),
            '{' => tokens.append(.{
                .position = self.cursor,
                .kind = .OpenBrace,
            }),
            '}' => tokens.append(.{
                .position = self.cursor,
                .kind = .CloseBrace,
            }),
            '[' => tokens.append(.{
                .position = self.cursor,
                .kind = .OpenBracket,
            }),
            ']' => tokens.append(.{
                .position = self.cursor,
                .kind = .CloseBracket,
            }),
            '\\' => self.scanBackslash(&tokens),
            else => tokens.append(.{
                .position = self.cursor,
                .kind = .{ .Character = self.pattern[self.cursor] },
            }),
        };
    }
    return tokens.toOwnedSlice();
}

fn is_special(ch: u8) bool {
    return ch == '^' or ch == '$' or ch == '.' or ch == '*' or ch == '?';
}

fn scanBackslash(self: *Lexer, tokens: *std.ArrayList(Token)) !void {
    const nextChar = try self.next();
    defer _ = self.eatSingle() catch {};
    try switch (nextChar) {
        '\\' => tokens.append(.{
            .position = self.cursor,
            .kind = .{ .Character = '\\' },
        }),
        'a'...'z' => |ch| {
            try switch (ch) {
                's' => tokens.append(.{ .position = self.cursor, .kind = .Space }),
                'w' => tokens.append(.{ .position = self.cursor, .kind = .Word }),
                'n' => tokens.append(.{ .position = self.cursor, .kind = .Line }),
                't' => tokens.append(.{ .position = self.cursor, .kind = .Tab }),
                else => tokens.append(.{
                    .position = self.cursor,
                    .kind = .{ .LiteralCharacter = ch },
                }),
            };
        },
        else => |ch| {
            if (is_special(ch)) {
                try tokens.append(.{
                    .position = self.cursor,
                    .kind = .{ .Character = ch },
                });
            }
        },
    };
}

fn eatSingle(self: *Lexer) !void {
    if (self.cursor + 1 >= self.pattern.len) {
        return error.StringOverflow;
    }
    self.cursor += 1;
}

fn eat(self: *Lexer, count: u32) !void {
    if (self.cursor + count >= self.pattern.len) {
        return error.StringOverflow;
    }
    self.cursor += count;
}

fn next(self: *Lexer) !u8 {
    if (self.cursor + 1 >= self.pattern.len) {
        return error.OutofBounds;
    }
    return self.pattern[self.cursor + 1];
}
