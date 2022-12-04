const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var first_half_char_present: [52]bool = [_]bool{false} ** 52;
    var second_half_char_present: [52]bool = [_]bool{false} ** 52;
    var total_priority: u64 = 0;

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len % 2 != 0) {
            std.debug.panic("line isn't even", .{});
        }

        first_half_char_present = [_]bool{false} ** 52;
        second_half_char_present = [_]bool{false} ** 52;

        var half = line.len / 2;
        for (line) |c, idx| {
            var priority = charToPriority(c) - 1;
            if (idx < half) {
                first_half_char_present[priority] = true;
            } else {
                second_half_char_present[priority] = true;
            }
        }

        var idx: u64 = 0;
        while (idx < 52) {
            if (first_half_char_present[idx] and second_half_char_present[idx]) {
                total_priority += idx + 1;
            }
            idx += 1;
        }
    }

    std.log.info("total priority is {d}", .{total_priority});
}

fn charToPriority(u: u8) u8 {
    if (std.ascii.isUpper(u)) {
        return u - 38;
    } else if (std.ascii.isLower(u)) {
        return u - 96;
    } else {
        std.debug.panic("char is not letter: {c}", .{u});
    }
}
