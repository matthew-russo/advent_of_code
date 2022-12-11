const std = @import("std");

pub fn main() !void {
    var allocator_provider = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = allocator_provider.allocator();

    var file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();
    var lines = std.ArrayList([]const u8).init(allocator);

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line_borrow| {
        var line = allocator.alloc(u8, line_borrow.len) catch unreachable;
        std.mem.copy(u8, line, line_borrow);
        lines.append(line) catch unreachable;
    }

    var num_cols = lines.items[0].len;
    var num_rows = lines.items.len;

    var highest_scenic_score: u64 = 0;

    var row_idx: u64 = 0;
    while (row_idx < num_rows) {
        var col_idx: u64 = 0;
        while (col_idx < num_cols) {
            if (row_idx == 0 or row_idx == num_rows - 1 or col_idx == 0 or col_idx == num_cols - 1) {
                col_idx += 1;
                continue;
            }

            var curr = charToNum(lines.items[row_idx][col_idx]);

            var left: u64 = 0;
            var right: u64 = 0;
            var up: u64 = 0;
            var down: u64 = 0;

            var temp_checker: i64 = @intCast(i64, col_idx) - 1;
            while (temp_checker >= 0) {
                var other = charToNum(lines.items[row_idx][@intCast(usize, temp_checker)]);
                if (other >= curr) {
                    left = col_idx - @intCast(u64, temp_checker);
                    break;
                }
                temp_checker -= 1;
            }
            if (left == 0) {
                left = col_idx;
            }

            temp_checker = @intCast(i64, col_idx) + 1;
            while (temp_checker < num_cols) {
                var other = charToNum(lines.items[row_idx][@intCast(usize, temp_checker)]);
                if (other >= curr) {
                    right = @intCast(u64, temp_checker) - col_idx;
                    break;
                }
                temp_checker += 1;
            }
            if (right == 0) {
                right = num_cols - col_idx - 1;
            }

            temp_checker = @intCast(i64, row_idx) - 1;
            while (temp_checker >= 0) {
                var other = charToNum(lines.items[@intCast(usize, temp_checker)][col_idx]);
                if (other >= curr) {
                    up = row_idx - @intCast(u64, temp_checker);
                    break;
                }
                temp_checker -= 1;
            }
            if (up == 0) {
                up = row_idx;
            }

            temp_checker = @intCast(i64, row_idx) + 1;
            while (temp_checker < num_rows) {
                var other = charToNum(lines.items[@intCast(usize, temp_checker)][col_idx]);
                if (other >= curr) {
                    down = @intCast(u64, temp_checker) - row_idx;
                    break;
                }
                temp_checker += 1;
            }
            if (down == 0) {
                down = num_rows - row_idx - 1;
            }

            var scenic_score = left * right * up * down;
            if (scenic_score > highest_scenic_score) {
                highest_scenic_score = scenic_score;
            }

            col_idx += 1;
        }
        row_idx += 1;
    }

    std.log.info("highest scenic score is {d}", .{highest_scenic_score});
}

fn charToNum(c: u8) u8 {
    return switch (c) {
        '0' => 0,
        '1' => 1,
        '2' => 2,
        '3' => 3,
        '4' => 4,
        '5' => 5,
        '6' => 6,
        '7' => 7,
        '8' => 8,
        '9' => 9,
        else => std.debug.panic("not a number: {c}", .{c}),
    };
}
