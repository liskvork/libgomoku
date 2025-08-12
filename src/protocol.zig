const std = @import("std");

const game = @import("gomoku_game");

const SliceError = error{
    SliceTooSmall,
    NonWhitespaceInTrim,
};

fn all_respect(T: type, slice: []const T, predicate: fn (val: T) bool) bool {
    for (slice) |i| {
        if (!predicate(i))
            return false;
    }
    return true;
}

inline fn is_all_whitespace(slice: []const u8) bool {
    return all_respect(u8, slice, std.ascii.isWhitespace);
}

fn skip_n_whitespace(slice: []const u8, n: usize) ![]const u8 {
    if (slice.len < n)
        return SliceError.SliceTooSmall;
    if (!is_all_whitespace(slice[0..n]))
        return SliceError.NonWhitespaceInTrim;
    return slice[n..];
}

pub const ClientInfo = struct {
    const Self = @This();

    k: []const u8,
    v: []const u8,

    pub fn init(allocator: std.mem.Allocator, k: []const u8, v: []const u8) !Self {
        return .{
            .k = try allocator.dupe(u8, k),
            .v = try allocator.dupe(u8, v),
        };
    }

    pub fn deinit(self: *const Self, allocator: std.mem.Allocator) void {
        allocator.free(self.k);
        allocator.free(self.v);
    }
};

pub const ClientCommandLogType = enum {
    Info,
    Debug,
    Error,
    Unknown,
};

pub const ClientCommandLog = struct {
    const Self = @This();

    msg_type: ClientCommandLogType,
    data: []const u8,
    allocator: std.mem.Allocator,

    fn init(log_type: ClientCommandLogType, data: []const u8, allocator: std.mem.Allocator) !ClientCommandLog {
        return .{
            .msg_type = log_type,
            .data = try allocator.dupe(u8, data),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: Self) void {
        self.allocator.free(self.data);
    }
};

const ClientResponseKO = struct {
    const Self = @This();

    data: ?[]const u8,
    allocator: std.mem.Allocator,

    fn init(data: ?[]const u8, allocator: std.mem.Allocator) !Self {
        return .{
            .data = if (data) |d| try allocator.dupe(u8, d) else null,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: Self) void {
        if (self.data) |d|
            self.allocator.free(d);
    }
};

pub const ClientCommand = union(enum) {
    const Self = @This();

    CommandLog: ClientCommandLog,
    ResponseOK: void,
    ResponseKO: ClientResponseKO,
    ResponsePosition: game.Position,
    ResponseAbout: std.ArrayList(ClientInfo),

    pub fn deinit(self: Self) void {
        switch (self) {
            .CommandLog => |v| v.deinit(),
            .ResponseKO => |v| v.deinit(),
            else => {},
        }
    }
};

const log_starters = [_][]const u8{
    "MESSAGE",
    "DEBUG",
    "ERROR",
    "UNKNOWN",
};

fn parse_log(msg: []const u8, idx: usize, allocator: std.mem.Allocator) !?ClientCommand {
    const starter = log_starters[idx];
    const ws_data = skip_n_whitespace(msg[starter.len..], 1) catch {
        return null;
    };

    const data = std.mem.trim(u8, ws_data, &std.ascii.whitespace);

    if (data.len == 0)
        return null;
    return .{
        .CommandLog = try ClientCommandLog.init(
            @enumFromInt(idx),
            data,
            allocator,
        ),
    };
}

fn parse_ok(msg: []const u8) ?ClientCommand {
    const rest = msg[2..]; // Skip the start
    if (!is_all_whitespace(rest))
        return null;
    return .ResponseOK;
}

fn parse_ko(msg: []const u8, allocator: std.mem.Allocator) !?ClientCommand {
    const rest = msg[2..];

    // Check if there is a message with the KO
    if (is_all_whitespace(rest))
        return .{
            .ResponseKO = try ClientResponseKO.init(null, allocator),
        };

    const ws_data = skip_n_whitespace(rest, 1) catch {
        return null;
    };

    const data = std.mem.trim(u8, ws_data, &std.ascii.whitespace);
    return .{
        .ResponseKO = try ClientResponseKO.init(data, allocator),
    };
}

// That is so ugly but I don't really have another idea right now
// TODO: Make so it isn't complete garbage
fn about_cleanup(out: *std.ArrayList(ClientInfo), allocator: std.mem.Allocator) ?ClientCommand {
    for (out.items) |i|
        i.deinit(allocator);
    out.deinit();
    return null;
}

// TODO: Same here this is ultra ugly
//
// Parses something like the following (taken directly from the documentation)
// name="SmortBrain",version="1.0",author="emneo",country="FR",www="emneo.dev"
fn parse_about_response(msg: []const u8, allocator: std.mem.Allocator) !?ClientCommand {
    var result = std.ArrayList(ClientInfo).init(allocator);
    var rest = std.mem.trim(u8, msg, &std.ascii.whitespace);
    while (rest.len > 0) {
        const equal_idx = std.mem.indexOf(u8, rest, "=") orelse return about_cleanup(&result, allocator);
        const k = std.mem.trim(u8, rest[0..equal_idx], &std.ascii.whitespace);
        rest = rest[equal_idx + 1 ..];
        const start_quote = std.mem.indexOf(u8, rest, "\"") orelse return about_cleanup(&result, allocator);
        rest = rest[start_quote + 1 ..];
        const end_quote = std.mem.indexOf(u8, rest, "\"") orelse return about_cleanup(&result, allocator);
        const v = rest[0..end_quote];
        rest = std.mem.trim(u8, rest[end_quote + 1 ..], &std.ascii.whitespace);
        try result.append(try ClientInfo.init(allocator, k, v));
        if (rest.len == 0)
            continue;
        const next_comma = std.mem.indexOf(u8, rest, ",") orelse return about_cleanup(&result, allocator);
        rest = std.mem.trim(u8, rest[next_comma + 1 ..], &std.ascii.whitespace);
    }
    if (result.items.len == 0)
        return about_cleanup(&result, allocator);
    return .{
        .ResponseAbout = result,
    };
}

fn parse_turn(msg: []const u8) ?ClientCommand {
    const num_commas = std.mem.count(u8, msg, ",");
    if (num_commas != 1)
        return null;
    const comma_idx = std.mem.indexOf(u8, msg, ",");
    // The comma is at the end of the message, can't parse
    if (comma_idx.? + 1 == msg.len)
        return null;
    const first_num_slice = std.mem.trim(u8, msg[0..comma_idx.?], &std.ascii.whitespace);
    const second_num_slice = std.mem.trim(u8, msg[comma_idx.? + 1 ..], &std.ascii.whitespace);
    const first_num = std.fmt.parseInt(u32, first_num_slice, 10) catch return null;
    const second_num = std.fmt.parseInt(u32, second_num_slice, 10) catch return null;
    return ClientCommand{
        .ResponsePosition = .{ first_num, second_num },
    };
}

pub fn parse(msg: []const u8, allocator: std.mem.Allocator) !?ClientCommand {
    for (log_starters, 0..) |s, i| {
        if (std.mem.startsWith(u8, msg, s))
            return try parse_log(msg, i, allocator);
    }
    if (std.mem.startsWith(u8, msg, "OK"))
        return parse_ok(msg);
    if (std.mem.startsWith(u8, msg, "KO"))
        return try parse_ko(msg, allocator);
    const tmp = try parse_about_response(msg, allocator);
    if (tmp != null)
        return tmp;
    return parse_turn(msg);
}

// -------------------------
// --------- TESTS ---------
// -------------------------

test "about name version www with extra character" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse(
        "name   =\"    funny    \",\t\t      \t version\t =  \"1\t. 0\",www =       \"em\tneo.dev\" :3",
        alloc,
    );
    try t.expect(cmd == null);
}

test "about name version www" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse(
        "name   =\"    funny    \",\t\t      \t version\t =  \"1\t. 0\",www =       \"em\tneo.dev\"",
        alloc,
    );
    try t.expect(cmd != null);
    defer {
        for (cmd.?.ResponseAbout.items) |i|
            i.deinit(alloc);
        cmd.?.ResponseAbout.deinit();
    }

    var expected = std.ArrayList(ClientInfo).init(alloc);
    defer expected.deinit();
    try expected.appendSlice(&.{
        .{ .k = "name", .v = "    funny    " },
        .{ .k = "version", .v = "1\t. 0" },
        .{ .k = "www", .v = "em\tneo.dev" },
    });

    try t.expectEqualDeep(cmd, ClientCommand{
        .ResponseAbout = expected,
    });
}

test "turn 0,0" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("0,0", alloc);
    try t.expect(cmd != null);

    try t.expectEqualDeep(cmd, ClientCommand{
        .ResponsePosition = .{ 0, 0 },
    });
}

test "turn 13,42" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("13  \t ,  \t \t42", alloc);
    try t.expect(cmd != null);

    try t.expectEqualDeep(cmd, ClientCommand{
        .ResponsePosition = .{ 13, 42 },
    });
}

test "turn 0,0,0 fail" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("0,0,0", alloc);
    try t.expect(cmd == null);
}

test "about version www" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("version=\"1.0\",www=\"emneo.dev\"", alloc);
    try t.expect(cmd != null);
    defer {
        for (cmd.?.ResponseAbout.items) |i|
            i.deinit(alloc);
        cmd.?.ResponseAbout.deinit();
    }

    var expected = std.ArrayList(ClientInfo).init(alloc);
    defer expected.deinit();
    try expected.appendSlice(&.{
        .{ .k = "version", .v = "1.0" },
        .{ .k = "www", .v = "emneo.dev" },
    });

    try t.expectEqualDeep(cmd, ClientCommand{
        .ResponseAbout = expected,
    });
}

test "about just name" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("name=\"funny\"", alloc);
    try t.expect(cmd != null);
    defer {
        for (cmd.?.ResponseAbout.items) |i|
            i.deinit(alloc);
        cmd.?.ResponseAbout.deinit();
    }

    var expected = std.ArrayList(ClientInfo).init(alloc);
    defer expected.deinit();
    try expected.appendSlice(&.{.{ .k = "name", .v = "funny" }});

    try t.expectEqualDeep(cmd, ClientCommand{
        .ResponseAbout = expected,
    });
}

test "ko parsing" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("KO", alloc);

    try t.expectEqualDeep(cmd, ClientCommand{
        .ResponseKO = .{
            .data = null,
            .allocator = alloc,
        },
    });
}

test "ko parsing with whitespace" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("KO   \t\t    \t     ", alloc);

    try t.expectEqualDeep(cmd, ClientCommand{
        .ResponseKO = .{
            .data = null,
            .allocator = alloc,
        },
    });
}

test "ko parsing with data" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("KO YEAH THERE ARE SOME THINGS HERE", alloc);
    try t.expect(cmd != null);
    defer cmd.?.deinit();

    try t.expectEqualDeep(cmd, ClientCommand{
        .ResponseKO = .{
            .data = "YEAH THERE ARE SOME THINGS HERE",
            .allocator = alloc,
        },
    });
}

test "bad ko parsing" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("KOYEAH THERE ARE SOME THINGS HERE", alloc);

    try t.expectEqual(cmd, null);
}

test "ok parsing" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("OK", alloc);

    try t.expectEqualDeep(cmd, .ResponseOK);
}

test "ok parsing with whitespace" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("OK   \t\t    \t     ", alloc);

    try t.expectEqualDeep(cmd, .ResponseOK);
}

test "bad ok parsing" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("OK YEAH THERE ARE SOME THINGS HERE", alloc);

    try t.expectEqual(cmd, null);
}

test "bad ok parsing 2" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("OKYEAH THERE ARE SOME THINGS HERE", alloc);

    try t.expectEqual(cmd, null);
}

test "debug log parsing" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("DEBUG issou that works", alloc);
    try t.expect(cmd != null);

    defer cmd.?.deinit();

    try t.expectEqualDeep(cmd, ClientCommand{
        .CommandLog = .{
            .msg_type = ClientCommandLogType.Debug,
            .data = "issou that works",
            .allocator = alloc,
        },
    });
}

test "info log parsing with whitespace" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("MESSAGE \t  \t      issou that works", alloc);
    try t.expect(cmd != null);

    defer cmd.?.deinit();

    try t.expectEqualDeep(cmd, ClientCommand{
        .CommandLog = .{
            .msg_type = ClientCommandLogType.Info,
            .data = "issou that works",
            .allocator = alloc,
        },
    });
}

test "error log parsing no msg" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("ERROR", alloc);
    try t.expect(cmd == null);
}

test "error log parsing with garbage" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("ERRORILOVEFOODABITTOOMUCH", alloc);
    try t.expect(cmd == null);
}

test "unknown log parsing one whitespace" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("UNKNOWN ", alloc);
    try t.expect(cmd == null);
}

test "debug log parsing multiple whitespace" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("DEBUG     \t    \t        ", alloc);
    try t.expect(cmd == null);
}

test "error log parsing with whitespace" {
    const t = std.testing;
    const alloc = t.allocator;

    const cmd = try parse("ERROR\t  \t      issou that works", alloc);
    try t.expect(cmd != null);

    defer cmd.?.deinit();

    try t.expectEqualDeep(cmd, ClientCommand{
        .CommandLog = .{
            .msg_type = ClientCommandLogType.Error,
            .data = "issou that works",
            .allocator = alloc,
        },
    });
}
