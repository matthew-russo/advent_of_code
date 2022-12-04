const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var inclusive_ranges: u64 = 0;

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var ranges_spliterator = std.mem.split(u8, line, ",");
        var first_range = ranges_spliterator.next() orelse unreachable;
        var second_range = ranges_spliterator.next() orelse unreachable;
        if (ranges_spliterator.next()) |_| {
            std.debug.panic("more than 2 ranges in a single line. {s}", .{line});
        }

        var first_range_spliterator = std.mem.split(u8, first_range, "-");
        var first_range_low_str = first_range_spliterator.next() orelse unreachable;
        var first_range_low = try std.fmt.parseInt(u64, first_range_low_str, 10);
        var first_range_high_str = first_range_spliterator.next() orelse unreachable;
        var first_range_high = try std.fmt.parseInt(u64, first_range_high_str, 10);
        if (first_range_spliterator.next()) |_| {
            std.debug.panic("more than 2 numbers in a range. {s}", .{first_range});
        }

        var second_range_spliterator = std.mem.split(u8, second_range, "-");
        var second_range_low_str = second_range_spliterator.next() orelse unreachable;
        var second_range_low = try std.fmt.parseInt(u64, second_range_low_str, 10);
        var second_range_high_str = second_range_spliterator.next() orelse unreachable;
        var second_range_high = try std.fmt.parseInt(u64, second_range_high_str, 10);
        if (second_range_spliterator.next()) |_| {
            std.debug.panic("more than 2 numbers in a range. {s}", .{second_range});
        }

        if (first_range_low < second_range_low) {
            if (first_range_high >= second_range_high) {
                inclusive_ranges += 1;
            }
        } else if (first_range_low > second_range_low) {
            if (first_range_high <= second_range_high) {
                inclusive_ranges += 1;
            }
        } else {
            inclusive_ranges += 1;
        }
    }

    std.log.info("number of ranges is {d}", .{inclusive_ranges});
}
