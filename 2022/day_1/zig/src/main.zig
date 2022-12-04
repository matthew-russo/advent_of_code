const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var highest_one: u64 = 0;
    var highest_two: u64 = 0;
    var highest_three: u64 = 0;
    var curr: u64 = 0;

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len != 0) {
            var n = try std.fmt.parseInt(u64, line, 10);
            curr += n;
        } else {
            if (curr >= highest_one) {
                var old_highest_one = highest_one;
                var old_highest_two = highest_two;
                highest_one = curr;
                highest_two = old_highest_one;
                highest_three = old_highest_two;
            } else if (curr >= highest_two) {
                var old_highest_two = highest_two;
                highest_two = curr;
                highest_three = old_highest_two;
            } else if (curr > highest_three) {
                highest_three = curr;
            }
            curr = 0;
        }
    }

    std.log.info("top 1 is {d}", .{highest_one});
    std.log.info("top 3 are {d}", .{highest_one + highest_two + highest_three});
}
