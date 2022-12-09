const std = @import("std");

pub fn main() !void {
    var allocator_provider = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = allocator_provider.allocator();

    var file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var present: [52]bool = [_]bool{false} ** 52;
    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf = try in_stream.readAllAlloc(allocator, std.math.maxInt(usize));
    var idx: usize = 14;
    outer: while (idx < buf.len) {
        present = [_]bool{false} ** 52;

        var curr: usize = 0;
        while (curr < 14) {
            var prio = charToPriority(buf[idx - curr]);
            if (present[prio]) {
                idx += 1;
                continue :outer;
            }
            present[prio] = true;
            curr += 1;
        }

        std.log.info("marker is at: {d}", .{idx + 1});
        return;
    }

    std.log.info("marker not found", .{});
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
