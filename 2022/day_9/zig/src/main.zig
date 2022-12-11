const std = @import("std");

const Position = struct {
    x: i64,
    y: i64,
};

pub fn main() !void {
    var allocator_provider = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = allocator_provider.allocator();

    var file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var head_x: i64 = 0;
    var head_y: i64 = 0;
    var tail_x: i64 = 0;
    var tail_y: i64 = 0;

    var visited_tail_positions = std.AutoHashMap(Position, void).init(allocator);
    visited_tail_positions.put(Position{ .x = tail_x, .y = tail_y }, {}) catch unreachable;

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var spliterator = std.mem.tokenize(u8, line, " ");
        var dir = spliterator.next() orelse unreachable;
        var magnitude_str = spliterator.next() orelse unreachable;
        var magnitude = std.fmt.parseInt(u64, magnitude_str, 10) catch unreachable;

        var i: u64 = 0;
        while (i < magnitude) {
            if (std.mem.eql(u8, dir, "R")) {
                head_x += 1;
            } else if (std.mem.eql(u8, dir, "L")) {
                head_x -= 1;
            } else if (std.mem.eql(u8, dir, "U")) {
                head_y += 1;
            } else if (std.mem.eql(u8, dir, "D")) {
                head_y -= 1;
            } else {
                std.debug.panic("unknow direction: {s}", .{dir});
            }

            if (head_x - tail_x > 1) {
                // need to move tail to the right
                if (head_y - tail_y == 1) {
                    // need to move diagonaly up-right
                    tail_y += 1;
                    tail_x += 1;
                } else if (tail_y - head_y == 1) {
                    // need to move diagonaly down-right
                    tail_y -= 1;
                    tail_x += 1;
                } else {
                    // just move right
                    tail_x += 1;
                }
            } else if (tail_x - head_x > 1) {
                // need to move tail to the left
                if (head_y - tail_y == 1) {
                    // need to move diagonaly up-left
                    tail_y += 1;
                    tail_x -= 1;
                } else if (tail_y - head_y == 1) {
                    // need to move diagonaly down-left
                    tail_y -= 1;
                    tail_x -= 1;
                } else {
                    // just move left
                    tail_x -= 1;
                }
            } else if (head_y - tail_y > 1) {
                // need to move tail up
                if (head_x - tail_x == 1) {
                    // need to move diagonally up-right
                    tail_y += 1;
                    tail_x += 1;
                } else if (tail_x - head_x == 1) {
                    // need to move diagonally up-left
                    tail_y += 1;
                    tail_x -= 1;
                } else {
                    // just move up
                    tail_y += 1;
                }
            } else if (tail_y - head_y > 1) {
                // need to move tail down
                if (head_x - tail_x == 1) {
                    // need to move diagonally down-right
                    tail_y -= 1;
                    tail_x += 1;
                } else if (tail_x - head_x == 1) {
                    // need to move diagonally down-left
                    tail_y -= 1;
                    tail_x -= 1;
                } else {
                    // just move down
                    tail_y -= 1;
                }
            }

            // std.log.info("tail: ({d}, {d})", .{ tail_x, tail_y });

            // record tail location
            visited_tail_positions.put(Position{ .x = tail_x, .y = tail_y }, {}) catch unreachable;

            i += 1;
        }
    }
    // std.log.info("", .{});
    // std.log.info("", .{});

    // var iter = visited_tail_positions.iterator();
    // while (iter.next()) |kvp| {
    //     std.log.info("visited: ({d}, {d})", .{ kvp.key_ptr.*.x, kvp.key_ptr.*.y });
    // }

    std.log.info("visited tail positions is {d}", .{visited_tail_positions.count()});
}
