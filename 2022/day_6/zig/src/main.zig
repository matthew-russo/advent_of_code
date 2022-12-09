const std = @import("std");

pub fn main() !void {
    var allocator_provider = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = allocator_provider.allocator();

    var file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var one: u8 = 0;
    var two: u8 = 0;
    var three: u8 = 0;
    var four: u8 = 0;

    var buf = try in_stream.readAllAlloc(allocator, std.math.maxInt(usize));
    var idx: usize = 3;
    while (idx < buf.len) {
        one = buf[idx - 3];
        two = buf[idx - 2];
        three = buf[idx - 1];
        four = buf[idx];

        if (one != two and one != three and one != four and
            two != three and two != four and
            three != four)
        {
            std.log.info("marker is at: {d}", .{idx + 1});
            return;
        }

        idx += 1;
    }

    std.log.info("marker not found", .{});
}
