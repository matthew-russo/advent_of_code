const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var max: u64 = 0;
    var curr: u64 = 0;

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len != 0) {
            var n = try std.fmt.parseInt(u64, line, 10);
            curr += n;
        } else {
            if (curr > max) {
                max = curr;
            }
            curr = 0;
        }
    }

    std.log.info("max is {d}", .{max});
}
