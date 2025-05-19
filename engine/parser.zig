const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const Tuple = std.meta.Tuple;

const ParseError = error{UnexpectedEOF} || Allocator.Error || std.fmt.ParseIntError;

pub fn Result(comptime T: type) type {
    return struct {
        const Self = @This();
        index: usize,
        value: union(enum) {
            ok: T,
            err,
        },
        pub fn ok(index: usize, value: T) Self {
            return .{ .index = index, .value = .{ .ok = value } };
        }
        pub fn err(index: usize) Self {
            return .{ .index = index, .value = .err };
        }
    };
}

pub fn Parser(comptime _T: type) type {
    return struct {
        pub const T = _T;
        const Self = @This();
        parse: *const fn (Allocator, []const u8) callconv(.@"inline") ParseError!Result(T),
    };
}

pub fn ParserResult(comptime parser: type) type {
    return switch (@typeInfo(parser)) {
        .pointer => |p| p.child.T,
        else => parser.T,
    };
}

fn typeCheck(comptime parser: type) void {
    const err = "expected type 'Parser(T)', found '" ++ @typeName(parser) ++ "'";
    const inner = switch (@typeInfo(parser)) {
        .pointer => |p| p.child,
        else => parser,
    };
    if (@typeInfo(inner) != .@"struct") @compileError(err);
    if (!@hasDecl(inner, "T")) @compileError(err);
    if (@TypeOf(inner.T) != type) @compileError(err);
}

fn parsersTypes(comptime parsers: anytype) []const type {
    var types: []const type = &[_]type{};
    inline for (parsers) |parser| {
        const T = ParserResult(@TypeOf(parser));
        if (T != void) {
            types = types ++ [_]type{T};
        }
    }
    return types;
}

fn get_types(comptime n: usize, comptime types: [n]type) type {
    return Tuple(&types);
}

fn SeqType(comptime parsers: anytype) type {
    const types = parsersTypes(parsers);
    if (types.len == 0) {
        return void;
    }
    if (types.len == 1) {
        return types[0];
    }
    return get_types(types.len, types[0..types.len].*);
}

pub fn sequenceParser(comptime parsers: anytype) Parser(SeqType(parsers)) {
    inline for (parsers) |parser| typeCheck(@TypeOf(parser));
    const tuples_type = SeqType(parsers);
    const Res = Result(tuples_type);
    return .{
        .parse = struct {
            inline fn parse(allocator: Allocator, str: []const u8) ParseError!Res {
                var tuples: tuples_type = undefined;
                var index: usize = 0;
                comptime var i = 0;
                inline for (parsers) |parser| {
                    const result = try parser.parse(allocator, str[index..]);
                    switch (result.value) {
                        .err => return Res.err(result.index),
                        .ok => |val| tuples[i] = val,
                    }
                    i += 1;
                    index += result.index;
                }
                return Res.ok(index, tuples);
            }
        }.parse,
    };
}

pub fn anyOfParser(comptime parsers: anytype) Parser(ParserResult(@TypeOf(parsers[0]))) {
    inline for (parsers) |p| typeCheck(@TypeOf(p));
    const Res = Result(ParserResult(@TypeOf(parsers[0])));
    return .{
        .parse = struct {
            inline fn parse(allocator: Allocator, str: []const u8) ParseError!Res {
                var err_index: usize = 0;
                inline for (parsers) |parser| {
                    const res = try parser.parse(allocator, str);
                    switch (res.value) {
                        .ok => return res,
                        else => err_index = @max(err_index, res.index),
                    }
                }
                return Res.err(err_index);
            }
        }.parse,
    };
}

pub fn optionalParser(comptime parser: anytype) Parser(?ParserResult(@TypeOf(parser))) {
    const Res = Result(?ParserResult(@TypeOf(parser)));
    return .{
        .parse = struct {
            inline fn parse(allocator: Allocator, str: []const u8) ParseError!Res {
                const res = try parser.parse(allocator, str);
                return switch (res.value) {
                    .ok => |val| Res.ok(res.index, val),
                    .err => Res.ok(0, null),
                };
            }
        }.parse,
    };
}

pub fn oneOrMoreParser(comptime parser: anytype) Parser([]ParserResult(@TypeOf(parser))) {
    typeCheck(@TypeOf(parser));
    const ResultType = ParserResult(@TypeOf(parser));
    const max = std.math.maxInt(usize);
    const Res = Result([]ResultType);
    return .{ .parse = struct {
        inline fn parse(allocator: Allocator, str: []const u8) ParseError!Res {
            var index: usize = 0;
            var i: usize = 0;
            var results = std.ArrayList(ResultType).init(allocator);
            var res = try parser.parse(allocator, str[index..]);
            switch (res.value) {
                .ok => |val| try results.append(val),
                .err => return Res.err(index),
            }
            i += 1;
            index += res.index;
            while (i < max) : (i += 1) {
                res = try parser.parse(allocator, str[index..]);
                switch (res.value) {
                    .ok => |val| try results.append(val),
                    .err => break,
                }
                index += res.index;
            }
            return Res.ok(index, try results.toOwnedSlice());
        }
    }.parse };
}

pub fn stringParser(comptime str: []const u8) Parser([]const u8) {
    const Res = Result([]const u8);
    return .{
        .parse = struct {
            inline fn parse(_: Allocator, s: []const u8) ParseError!Res {
                if (!mem.startsWith(u8, s, str))
                    return Res.err(0);
                return Res.ok(str.len, str);
            }
        }.parse,
    };
}

const metaCharacters = [_][]const u8{
    "*?", "+?", "?", "*", "+",
};
const alphabets = [_][]const u8{
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
};

pub fn metaCharacterParser() Parser([]const u8) {
    const Res = Result([]const u8);
    return .{
        .parse = struct {
            inline fn parse(allocator: Allocator, s: []const u8) ParseError!Res {
                comptime var parsers: [metaCharacters.len]Parser([]const u8) = undefined;
                inline for (metaCharacters, 0..) |ch, i| {
                    parsers[i] = stringParser(ch);
                }
                return anyOfParser(
                    parsers,
                ).parse(allocator, s);
            }
        }.parse,
    };
}

fn isNumber(ch: u8) bool {
    return ch >= '0' and ch <= '9';
}

pub fn integerParser() Parser(usize) {
    const Res = Result(usize);
    return .{
        .parse = struct {
            inline fn parse(allocator: Allocator, str: []const u8) ParseError!Res {
                var i: usize = 0;
                var result = std.ArrayList(u8).init(allocator);
                defer result.deinit();
                while (i < str.len) : (i += 1) {
                    if (isNumber(str[i])) {
                        try result.append(str[i]);
                    } else break;
                }
                if (result.items.len == 0) return Res.err(0);
                return Res.ok(i, try std.fmt.parseInt(usize, result.items, 10));
            }
        }.parse,
    };
}

pub fn anyCharacterExceptMetaCharacters() Parser([]const u8) {
    const Res = Result([]const u8);
    return .{
        .parse = struct {
            inline fn parse(_: Allocator, str: []const u8) ParseError!Res {
                inline for (metaCharacters) |ch| {
                    if (mem.eql(u8, str, ch)) {
                        return Res.err(0);
                    }
                }
                return Res.ok(1, str);
            }
        }.parse,
    };
}

pub fn anyCharacterExceptSpecialCharacters() Parser([]const u8) {
    const Res = Result([]const u8);
    return .{
        .parse = struct {
            inline fn parse(allocator: Allocator, str: []const u8) ParseError!Res {
                comptime var parsers: [alphabets.len]Parser([]const u8) = undefined;
                inline for (alphabets, 0..) |alpha, i| {
                    parsers[i] = stringParser(alpha);
                }
                return anyOfParser(parsers).parse(allocator, str);
            }
        }.parse,
    };
}

pub fn characterParser() Parser([]const u8) {
    const Res = Result([]const u8);
    return .{
        .parse = struct {
            inline fn parse(allocator: Allocator, str: []const u8) ParseError!Res {
                return anyOfParser(&.{
                    &anyCharacterExceptMetaCharacters(),
                    &sequenceParser(&.{
                        &stringParser("\\"),
                        &anyCharacterExceptSpecialCharacters(),
                    }),
                }).parse(allocator, str);
            }
        }.parse,
    };
}

pub fn regex_parse() void {}
