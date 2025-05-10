const lexer = @import("lexer.zig");
const Token = lexer.Token;
const TokenType = lexer.TokenType;
const std = @import("std");
const Queue = @import("queue.zig").Queue;

const ParserError = error{
    InvalidToken,
    InvalidSyntax,
    NoClosingParen,
    NoClosingBracket,
    NoClosingBrace,
    StringOverflow,
    IntegerParse,
    LexerError,
} || std.mem.Allocator.Error;

const Diagnostics = struct {
    message: []const u8,
    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}", .{self.message});
    }
};

const Alt = struct {
    term: *Term,
    expr: ?*Alt = null,
    pub fn deinit(self: *Alt, allocator: std.mem.Allocator) void {
        self.term.deinit(allocator);
        if (self.expr) |expr| {
            expr.deinit(allocator);
            allocator.destroy(expr);
        }
    }
};

const Term = struct {
    factor: Factor,
    term: ?*Term = null,
    pub fn deinit(self: *Term, allocator: std.mem.Allocator) void {
        self.factor.deinit(allocator);
        if (self.term) |term| {
            term.deinit(allocator);
            allocator.destroy(term);
        }
    }
};

const Quantifier = enum {
    Star,
    Plus,
    Optional,
};

const Factor = struct {
    atom: *Atom,
    metachar: ?Quantifier = null,
    pub fn deinit(self: *Factor, allocator: std.mem.Allocator) void {
        self.atom.deinit(allocator);
        allocator.destroy(self.atom);
    }
};

const Atom = union(enum) {
    token: Token,
    expression: *Alt,
    chclasses: []CharacterClass,
    numberRange: NumberRange,
    pub fn deinit(self: *Atom, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .expression => |e| {
                e.deinit(allocator);
                allocator.destroy(e);
            },
            .chclasses => |cc| allocator.free(cc),
            else => {},
        }
    }
};

const CharacterClass = union(enum) {
    single: u8,
    range: CharacterRange,
};

const CharacterRange = struct {
    beginCharacter: u8,
    endCharacter: u8,
};

const NumberRange = struct {
    min: u32,
    max: ?u32 = null,
};

const Parser = @This();
tokens: []Token,
idx: u32 = 0,
allocator: std.mem.Allocator,
root: ?*Alt = null,

pub fn init(pattern: []const u8, allocator: std.mem.Allocator) ParserError!Parser {
    var l = lexer.init(pattern, allocator);
    const tokens = l.scan() catch |err| {
        const diag = Diagnostics{ .message = "Lexer Error." };
        std.debug.print("Message: {}\nType: {}\n", .{ diag, err });
        return ParserError.LexerError;
    };
    return .{
        .tokens = tokens,
        .allocator = allocator,
    };
}

pub fn deinit(self: *Parser) void {
    if (self.root) |root| {
        root.deinit(self.allocator);
        self.allocator.destroy(root);
    }
    self.allocator.free(self.tokens);
}

pub fn parse(self: *Parser) ParserError!void {
    self.root = try self.parseExpr();
}

fn parseExpr(self: *Parser) ParserError!?*Alt {
    const term = try self.parseTerm();
    if (term == null) return null;
    const expr = try self.allocator.create(Alt);
    errdefer self.allocator.destroy(expr);
    if (self.tokens[self.idx].kind == .Alter) {
        try self.eatToken();
        const alt = try self.parseExpr();
        expr.* = .{
            .term = term.?,
            .expr = alt,
        };
        return expr;
    }
    expr.* = .{ .term = term.? };
    return expr;
}

fn parseTerm(self: *Parser) ParserError!?*Term {
    const term_ptr = try self.allocator.create(Term);
    const factor = try self.parseFactor();
    if (factor == null) {
        return null;
    }
    const term = try self.parseTerm();
    if (term) |t| {
        term_ptr.* = .{
            .factor = factor.?,
            .term = t,
        };
    } else {
        term_ptr.* = .{
            .factor = factor.?,
        };
    }
    return term_ptr;
}

fn getmetachar(token: Token) ?Quantifier {
    switch (token.kind) {
        .Star => return Quantifier.Star,
        .Plus => return Quantifier.Plus,
        .Question => return Quantifier.Optional,
        else => return null,
    }
}

fn parseFactor(self: *Parser) ParserError!?Factor {
    const atom = try self.parseAtom();
    if (atom) |a| {
        const metachar = getmetachar(self.tokens[self.idx]);
        if (metachar) |meta| {
            return .{
                .atom = a,
                .metachar = meta,
            };
        }
        return .{ .atom = a };
    }
    return null;
}

fn parseAtom(self: *Parser) ParserError!?*Atom {
    const atom = try self.allocator.create(Atom);
    errdefer self.allocator.destroy(atom);
    if (self.tokens[self.idx].kind == .OpenParen) {
        try self.eatToken();
        const alt = try self.parseExpr();
        if (self.tokens[self.idx].kind == .CloseParen) {
            try self.eatToken();
        } else {
            return ParserError.NoClosingParen;
        }
        if (alt) |a| {
            atom.* = .{
                .expression = a,
            };
            return atom;
        }
        return null;
    } else if (self.tokens[self.idx].kind == .OpenBracket) {
        try self.eatToken();
        const chclasses = try self.parseCharacterClasses();
        if (self.tokens[self.idx].kind == .CloseBracket) {
            try self.eatToken();
        } else {
            return ParserError.NoClosingBracket;
        }
        atom.* = .{
            .chclasses = chclasses,
        };
        return atom;
    } else if (self.tokens[self.idx].kind == .OpenBrace) {
        try self.eatToken();
        const numberRange = try self.parseNumberRange();
        if (self.tokens[self.idx].kind == .CloseBrace) {
            try self.eatToken();
        } else {
            return ParserError.NoClosingBrace;
        }

        if (numberRange) |nr| {
            atom.* = .{
                .numberRange = nr,
            };
            return atom;
        }
        return null;
    } else if (self.tokens[self.idx].kind == .Space or
        self.tokens[self.idx].kind == .Word or
        self.tokens[self.idx].kind == .Line or
        self.tokens[self.idx].kind == .Tab or
        self.tokens[self.idx].kind == .Character)
    {
        const token = self.tokens[self.idx];
        try self.eatToken();
        atom.* = .{
            .token = token,
        };
        return atom;
    }
    return null;
}

fn parseCharacterClasses(self: *Parser) ParserError![]CharacterClass {
    var chclasses = std.ArrayList(CharacterClass).init(self.allocator);
    while (true) {
        const chclass = try self.parseCharacterClass();
        if (chclass) |cc| {
            try chclasses.append(cc);
        } else {
            break;
        }
    }
    return try chclasses.toOwnedSlice();
}

fn parseCharacterClass(self: *Parser) ParserError!?CharacterClass {
    const range = try self.parseCharacterRange();
    if (range) |r| {
        return .{
            .range = r,
        };
    }
    const single = try self.parseSingleCharacter();
    if (single) |s| {
        return .{
            .single = s,
        };
    }
    return null;
}

fn parseCharacterRange(self: *Parser) ParserError!?CharacterRange {
    var beginCharacter: u8 = undefined;
    var endCharacter: u8 = undefined;
    var tok = self.tokens[self.idx];

    switch (tok.kind) {
        .Character, .LiteralCharacter => |ch| beginCharacter = ch,
        else => return null,
    }
    tok = self.tokens[self.idx + 1];
    switch (tok.kind) {
        .Character => |ch| {
            if (ch == '-') {
                try self.eatToken();
            } else return null;
        },
        .LiteralCharacter => return null,
        else => return ParserError.InvalidToken,
    }
    try self.eatToken();
    try self.eatToken();
    tok = self.tokens[self.idx];
    switch (tok.kind) {
        .Character => |ch| endCharacter = ch,
        .CloseBracket => return ParserError.InvalidSyntax,
        else => return ParserError.InvalidToken,
    }
    return .{
        .beginCharacter = beginCharacter,
        .endCharacter = endCharacter,
    };
}

fn parseSingleCharacter(self: *Parser) ParserError!?u8 {
    const tok = self.tokens[self.idx];
    try self.eatToken();
    switch (tok.kind) {
        .Character, .LiteralCharacter => |ch| return ch,
        else => return null,
    }
}

fn isdigit(ch: u8) bool {
    return ch >= '0' and ch <= '9';
}

fn parseNumberRange(self: *Parser) ParserError!?NumberRange {
    try self.eatToken();
    self.eatSpaces();
    var tok = self.tokens[self.idx];
    if (tok.kind == .CloseBrace) {
        return null;
    }
    const min = self.parseNumber() catch |err| {
        const diag = Diagnostics{ .message = "Cannot convert to integer" };
        std.debug.print("Message: {any}, Type: {any}", .{ diag.message, err });
        return ParserError.IntegerParse;
    };
    self.eatSpaces();
    tok = self.tokens[self.idx];
    switch (tok.kind) {
        .Character => |ch| {
            if (ch == ',') {
                try self.eatToken();
            } else return ParserError.InvalidSyntax;
        },
        .CloseBrace => return NumberRange{ .min = min },
        else => return ParserError.InvalidToken,
    }
    self.eatSpaces();
    tok = self.tokens[self.idx];
    if (tok.kind != .Character) {
        return ParserError.InvalidToken;
    }
    const max = self.parseNumber() catch |err| {
        const diag = Diagnostics{ .message = "Cannot convert to integer." };
        std.debug.print("Message: {any}\nType: {any}\n", .{ diag.message, err });
        return ParserError.IntegerParse;
    };
    self.eatSpaces();
    return .{
        .min = min,
        .max = max,
    };
}

fn parseNumber(self: *Parser) !u32 {
    var res = std.ArrayList(u8).init(self.allocator);
    while (self.idx < self.tokens.len) : (self.idx += 1) {
        const tok = self.tokens[self.idx];
        const ch = switch (tok.kind) {
            .Character => |c| c,
            else => break,
        };
        if (isdigit(ch)) {
            try res.append(ch);
        }
    }
    const res_t = try res.toOwnedSlice();
    defer self.allocator.free(res_t);
    return std.fmt.parseInt(u32, res_t, 10);
}

fn eatSpaces(self: *Parser) void {
    while (self.idx < self.tokens.len) : (self.idx += 1) {
        const tok = self.tokens[self.idx];
        const ch = switch (tok.kind) {
            .Character => |c| c,
            else => break,
        };
        if (ch != ' ') break;
    }
}

fn eatToken(self: *Parser) ParserError!void {
    if (self.idx >= self.tokens.len) {
        return ParserError.StringOverflow;
    }
    self.idx += 1;
}

const QueueItem = union(enum) {
    alt: *Alt,
    term: *Term,
    factor: Factor,
    atom: *Atom,
    chclass: *CharacterClass,
    chrange: CharacterRange,
    numrange: NumberRange,
    token: Token,
    ch: u8,
    meta: Quantifier,
};

fn repeatChars(ch: u8, count: u32, allocator: std.mem.Allocator) ![]u8 {
    var res = std.ArrayList(u8).init(allocator);
    for (0..count) |_| {
        try res.append(ch);
    }
    return try res.toOwnedSlice();
}

pub fn print(self: *Parser) !void {
    var queue = try Queue(QueueItem).init(self.allocator);
    defer queue.deinit();
    if (self.root) |root| {
        try queue.push(.{ .alt = root });
    }

    var indent: u32 = 48;
    var depth: u32 = 0;
    while (!queue.isempty()) {
        var i: u32 = 0;
        const len = queue.len();
        const spaces = try repeatChars(' ', indent, self.allocator);
        std.debug.print("{s}", .{spaces});
        while (i < len) : (i += 1) {
            const sub_spaces = try repeatChars(' ', 4, self.allocator);
            const node = try queue.pop();
            switch (node) {
                .alt => |a| {
                    std.debug.print("Alt{s}", .{sub_spaces});
                    try queue.push(.{ .term = a.term });
                    if (a.expr) |e| {
                        try queue.push(.{ .alt = e });
                    }
                },
                .term => |t| {
                    std.debug.print("Term{s}", .{sub_spaces});
                    try queue.push(.{ .factor = t.factor });
                    if (t.term) |term| {
                        try queue.push(.{ .term = term });
                    }
                },
                .factor => |f| {
                    std.debug.print("Factor{s}", .{sub_spaces});
                    try queue.push(.{ .atom = f.atom });
                    if (f.metachar) |meta| {
                        try queue.push(.{ .meta = meta });
                    }
                },
                .atom => |a| {
                    std.debug.print("Atom{s}", .{sub_spaces});
                    switch (a.*) {
                        .expression => |e| try queue.push(.{ .alt = e }),
                        .chclasses => |ccs| {
                            for (ccs) |*cc| {
                                try queue.push(.{ .chclass = cc });
                            }
                        },
                        .numberRange => |nr| try queue.push(.{ .numrange = nr }),
                        .token => |tok| try queue.push(.{ .token = tok }),
                    }
                },
                .chclass => |cc| {
                    std.debug.print("ChClass{s}", .{sub_spaces});
                    switch (cc.*) {
                        .single => |ch| try queue.push(.{ .ch = ch }),
                        .range => |r| try queue.push(.{ .chrange = r }),
                    }
                },
                .chrange => std.debug.print("ChRange{s}", .{sub_spaces}),
                .numrange => std.debug.print("NRange{s}", .{sub_spaces}),
                .token => std.debug.print("Token{s}", .{sub_spaces}),
                .ch => std.debug.print("Character{s}", .{sub_spaces}),
                .meta => |m| std.debug.print("{s}{s}", .{ @tagName(m), sub_spaces }),
            }
            self.allocator.free(sub_spaces);
        }
        indent -= 4;
        depth += 1;
        self.allocator.free(spaces);
        std.debug.print("\n", .{});
    }
}

test "Parser" {
    const allocator = std.testing.allocator;
    var parser = try Parser.init("(a|b)*", allocator);
    defer parser.deinit();
    try parser.parse();
    try parser.print();
}
