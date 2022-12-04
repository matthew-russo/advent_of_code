const std = @import("std");

const OPPONENT_ROCK: u8 = 'A';
const OPPONENT_PAPER: u8 = 'B';
const OPPONENT_SCISSORS: u8 = 'C';

const ME_ROCK: u8 = 'X';
const ME_PAPER: u8 = 'Y';
const ME_SCISSORS: u8 = 'Z';

const ROCK_POINTS: u64 = 1;
const PAPER_POINTS: u64 = 2;
const SCISSORS_POINTS: u64 = 3;

const LOSE_POINTS: u64 = 0;
const DRAW_POINTS: u64 = 3;
const WIN_POINTS: u64 = 6;

pub fn main() !void {
    var file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var score: u64 = 0;

    var buf: [4]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var opponent = line[0];
        var me = line[2];

        const points = switch (me) {
            ME_ROCK => blk: {
                var points = ROCK_POINTS;
                switch (opponent) {
                    OPPONENT_ROCK => break :blk points + DRAW_POINTS,
                    OPPONENT_PAPER => break :blk points + LOSE_POINTS,
                    OPPONENT_SCISSORS => break :blk points + WIN_POINTS,
                    else => std.debug.panic("unrecognized opponent operation", .{}),
                }
            },
            ME_PAPER => blk: {
                var points = PAPER_POINTS;
                switch (opponent) {
                    OPPONENT_ROCK => break :blk points + WIN_POINTS,
                    OPPONENT_PAPER => break :blk points + DRAW_POINTS,
                    OPPONENT_SCISSORS => break :blk points + LOSE_POINTS,
                    else => std.debug.panic("unrecognized opponent operation", .{}),
                }
            },
            ME_SCISSORS => blk: {
                var points = SCISSORS_POINTS;
                switch (opponent) {
                    OPPONENT_ROCK => break :blk points + LOSE_POINTS,
                    OPPONENT_PAPER => break :blk points + WIN_POINTS,
                    OPPONENT_SCISSORS => break :blk points + DRAW_POINTS,
                    else => std.debug.panic("unrecognized opponent operation", .{}),
                }
            },
            else => std.debug.panic("unrecognized me operation", .{}),
        };

        score += points;
    }

    std.log.info("total score is {d}", .{score});
}
