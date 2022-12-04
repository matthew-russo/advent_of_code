const std = @import("std");

const OPPONENT_ROCK: u8 = 'A';
const OPPONENT_PAPER: u8 = 'B';
const OPPONENT_SCISSORS: u8 = 'C';

const STRATEGY_LOSE: u8 = 'X';
const STRATEGY_DRAW: u8 = 'Y';
const STRATEGY_WIN: u8 = 'Z';

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
        var strategy = line[2];

        const points = switch (strategy) {
            STRATEGY_LOSE => blk: {
                var points = LOSE_POINTS;
                switch (opponent) {
                    OPPONENT_ROCK => break :blk points + SCISSORS_POINTS,
                    OPPONENT_PAPER => break :blk points + ROCK_POINTS,
                    OPPONENT_SCISSORS => break :blk points + PAPER_POINTS,
                    else => std.debug.panic("unrecognized opponent operation", .{}),
                }
            },
            STRATEGY_DRAW => blk: {
                var points = DRAW_POINTS;
                switch (opponent) {
                    OPPONENT_ROCK => break :blk points + ROCK_POINTS,
                    OPPONENT_PAPER => break :blk points + PAPER_POINTS,
                    OPPONENT_SCISSORS => break :blk points + SCISSORS_POINTS,
                    else => std.debug.panic("unrecognized opponent operation", .{}),
                }
            },
            STRATEGY_WIN => blk: {
                var points = WIN_POINTS;
                switch (opponent) {
                    OPPONENT_ROCK => break :blk points + PAPER_POINTS,
                    OPPONENT_PAPER => break :blk points + SCISSORS_POINTS,
                    OPPONENT_SCISSORS => break :blk points + ROCK_POINTS,
                    else => std.debug.panic("unrecognized opponent operation", .{}),
                }
            },
            else => std.debug.panic("unrecognized me operation", .{}),
        };

        score += points;
    }

    std.log.info("total score is {d}", .{score});
}
