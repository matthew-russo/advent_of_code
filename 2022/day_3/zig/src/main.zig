const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var first_elf_char_present: [52]bool = [_]bool{false} ** 52;
    var second_elf_char_present: [52]bool = [_]bool{false} ** 52;
    var third_elf_char_present: [52]bool = [_]bool{false} ** 52;
    var total_priority: u64 = 0;

    var buf: [1024]u8 = undefined;
    var lines_processed: u64 = 0;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        for (line) |c| {
            var priority = charToPriority(c) - 1;
            if (lines_processed % 3 == 0) {
                first_elf_char_present[priority] = true;
            } else if (lines_processed % 3 == 1) {
                second_elf_char_present[priority] = true;
            } else if (lines_processed % 3 == 2) {
                third_elf_char_present[priority] = true;
            } else {
                unreachable;
            }
        }

        if (lines_processed % 3 == 2) {
            var idx: u64 = 0;
            while (idx < 52) {
                if (first_elf_char_present[idx] and second_elf_char_present[idx] and third_elf_char_present[idx]) {
                    total_priority += idx + 1;
                }
                idx += 1;
            }

            // clear our counters
            first_elf_char_present = [_]bool{false} ** 52;
            second_elf_char_present = [_]bool{false} ** 52;
            third_elf_char_present = [_]bool{false} ** 52;
        }

        lines_processed += 1;
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
