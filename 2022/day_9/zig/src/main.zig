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

    var knots: [10]Position = [_]Position{Position{ .x = 0, .y = 0 }} ** 10;

    var visited_tail_positions = std.AutoHashMap(Position, void).init(allocator);
    visited_tail_positions.put(Position{ .x = 0, .y = 0 }, {}) catch unreachable;

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var spliterator = std.mem.tokenize(u8, line, " ");
        var dir = spliterator.next() orelse unreachable;
        var magnitude_str = spliterator.next() orelse unreachable;
        var magnitude = std.fmt.parseInt(u64, magnitude_str, 10) catch unreachable;

        var i: u64 = 0;
        while (i < magnitude) {
            var head = &knots[0];
            if (std.mem.eql(u8, dir, "R")) {
                head.*.x += 1;
            } else if (std.mem.eql(u8, dir, "L")) {
                head.*.x -= 1;
            } else if (std.mem.eql(u8, dir, "U")) {
                head.*.y += 1;
            } else if (std.mem.eql(u8, dir, "D")) {
                head.*.y -= 1;
            } else {
                std.debug.panic("unknow direction: {s}", .{dir});
            }

            var knot_idx: u64 = 1;
            while (knot_idx < 10) {
                var in_front = &knots[knot_idx - 1];
                var curr = &knots[knot_idx];

                if (in_front.*.x - curr.*.x > 1) {
                    // need to move tail to the right
                    if (in_front.*.y - curr.*.y >= 1) {
                        // need to move diagonaly up-right
                        curr.*.y += 1;
                        curr.*.x += 1;
                    } else if (curr.*.y - in_front.*.y >= 1) {
                        // need to move diagonaly down-right
                        curr.*.y -= 1;
                        curr.*.x += 1;
                    } else {
                        // just move right
                        curr.*.x += 1;
                    }
                } else if (curr.*.x - in_front.*.x > 1) {
                    // need to move tail to the left
                    if (in_front.*.y - curr.*.y >= 1) {
                        // need to move diagonaly up-left
                        curr.*.y += 1;
                        curr.*.x -= 1;
                    } else if (curr.*.y - in_front.*.y >= 1) {
                        // need to move diagonaly down-left
                        curr.*.y -= 1;
                        curr.*.x -= 1;
                    } else {
                        // just move left
                        curr.*.x -= 1;
                    }
                } else if (in_front.*.y - curr.*.y > 1) {
                    // need to move tail up
                    if (in_front.*.x - curr.*.x >= 1) {
                        // need to move diagonally up-right
                        curr.*.y += 1;
                        curr.*.x += 1;
                    } else if (curr.*.x - in_front.*.x >= 1) {
                        // need to move diagonally up-left
                        curr.*.y += 1;
                        curr.*.x -= 1;
                    } else {
                        // just move up
                        curr.*.y += 1;
                    }
                } else if (curr.*.y - in_front.*.y > 1) {
                    // need to move tail down
                    if (in_front.*.x - curr.*.x >= 1) {
                        // need to move diagonally down-right
                        curr.*.y -= 1;
                        curr.*.x += 1;
                    } else if (curr.*.x - in_front.*.x >= 1) {
                        // need to move diagonally down-left
                        curr.*.y -= 1;
                        curr.*.x -= 1;
                    } else {
                        // just move down
                        curr.*.y -= 1;
                    }
                }

                knot_idx += 1;
            }

            // record tail location
            var tail = knots[9];
            // std.log.info("tail: ({d}, {d})", .{ tail.x, tail.y });
            visited_tail_positions.put(Position{ .x = tail.x, .y = tail.y }, {}) catch unreachable;

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
