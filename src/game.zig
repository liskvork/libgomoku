const std = @import("std");
const Allocator = std.mem.Allocator;

// COLORS
const color_red: []const u8 = "\x1b[31m";
const color_blue: []const u8 = "\x1b[34m";
const color_green: []const u8 = "\x1b[32m";
const color_reset: []const u8 = "\x1b[m";

const player1_highlight = color_green ++ "O" ++ color_reset;
const player1_color = color_blue ++ "O" ++ color_reset;
const player2_highlight = color_green ++ "X" ++ color_reset;
const player2_color = color_red ++ "X" ++ color_reset;
// \COLORS

const CellState = enum {
    const Self = @This();

    Empty,
    Player1,
    Player2,

    pub fn to_slice(self: Self, colors: bool, highlight: bool) []const u8 {
        return switch (self) {
            .Empty => "-",
            .Player1 => if (!colors) "O" else if (highlight) player1_highlight else player1_color,
            .Player2 => if (!colors) "X" else if (highlight) player2_highlight else player2_color,
        };
    }
};

pub const MoveType = enum {
    const Self = @This();

    Player1,
    Player2,

    pub inline fn to_cell(self: Self) CellState {
        return switch (self) {
            .Player1 => .Player1,
            .Player2 => .Player2,
        };
    }

    pub inline fn from_idx(player_idx: usize) Self {
        return switch (player_idx) {
            0 => .Player1,
            1 => .Player2,
            else => unreachable,
        };
    }
};

pub const Position = @Vector(2, usize);
const Direction = @Vector(2, isize);

pub const Error = error{
    OutOfBound,
    AlreadyTaken,
};

pub const Game = struct {
    const Self = @This();
    const cells_to_align = 5;

    size: u32,
    board: []CellState,
    allocator: std.mem.Allocator,

    pub fn init(size: u32, allocator: std.mem.Allocator) Allocator.Error!Game {
        const b = try allocator.alloc(CellState, size * size);
        @memset(b, .Empty);
        return .{
            .size = size,
            .board = b,
            .allocator = allocator,
        };
    }

    inline fn reset(self: *Self) void {
        @memset(self.board, .Empty);
    }

    inline fn get_idx_from_pos(self: *const Self, pos: Position) usize {
        const idx = pos[0] + pos[1] * self.size;
        std.debug.assert(idx < self.board.len);
        return idx;
    }

    inline fn is_pos_inbound(self: *const Self, pos: Position) bool {
        const s = self.size;
        return pos[0] < s and pos[1] < s;
    }

    inline fn is_ipos_inbound(self: *const Self, pos: @Vector(2, isize)) bool {
        const s: isize = @intCast(self.size);
        return pos[0] >= 0 and pos[1] >= 0 and pos[0] < s and pos[1] < s;
    }

    inline fn at(self: *const Self, pos: Position) CellState {
        return self.board[self.get_idx_from_pos(pos)];
    }

    fn count_in_line(self: *const Self, start_pos: Position, direction: Direction, max_count: comptime_int) u32 {
        const state = self.at(start_pos);
        var current_pos = @as(Direction, @intCast(start_pos));

        inline for (0..max_count) |i| {
            if (!self.is_ipos_inbound(current_pos)) return i;

            const pos = @as(Position, @intCast(current_pos));
            if (self.at(pos) != state) return i;

            current_pos = current_pos + direction;
        }
        return max_count;
    }

    fn is_move_winning(self: *const Self, pos: Position) bool {
        const played_cell = self.at(pos);
        std.debug.assert(played_cell != .Empty);
        const directions = [_]Direction{
            .{ 1, 0 }, // Horizontal (-)
            .{ 0, 1 }, // Vertical (|)
            .{ 1, -1 }, // Main diagonal (\)
            .{ 1, 1 }, // Anti-diagonal (/)
        };

        inline for (directions) |dir| {
            const count = self.count_in_line(pos, dir, cells_to_align) +
                self.count_in_line(pos, -dir, cells_to_align) - 1;
            if (count >= cells_to_align) return true;
        }
        return false;
    }

    // Returns true if this was a winning move
    // false if the game continues
    pub fn place(self: *Self, pos: Position, move_type: MoveType) Error!bool {
        if (!is_pos_inbound(self, pos))
            return Error.OutOfBound;
        const idx = get_idx_from_pos(self, pos);
        if (self.board[idx] != .Empty)
            return Error.AlreadyTaken;
        self.board[idx] = move_type.to_cell();
        return self.is_move_winning(pos);
    }

    pub fn dump(self: *const Self, output_file: std.fs.File, colors: bool, pos_to_highlight: Position) !void {
        for (0..self.size) |x| {
            for (0..self.size) |y| {
                try output_file.writeAll(self.at(.{ x, y }).to_slice(
                    colors,
                    pos_to_highlight[0] == x and pos_to_highlight[1] == y,
                ));
            }
            try output_file.writeAll("\n");
        }
    }

    pub fn deinit(self: *const Self) void {
        self.allocator.free(self.board);
    }
};
