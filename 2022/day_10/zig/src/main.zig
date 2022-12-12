const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var cycles: i64 = 0;
    var reg: i64 = 1;

    var screen: [240]u8 = [_]u8{'.'} ** 240;
    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var spliterator = std.mem.tokenize(u8, line, " ");
        var cmd = spliterator.next() orelse unreachable;

        if (std.mem.eql(u8, cmd, "addx")) {
            var amt_str = spliterator.next() orelse unreachable;
            var amt = std.fmt.parseInt(i64, amt_str, 10) catch unreachable;

            var cycles_usize = @intCast(usize, cycles);
            var horiz_pos = @intCast(usize, @rem(cycles, 40));
            if (reg - 1 == horiz_pos) {
                screen[cycles_usize] = '#';
            } else if (reg == horiz_pos) {
                screen[cycles_usize] = '#';
            } else if (reg + 1 == horiz_pos) {
                screen[cycles_usize] = '#';
            }
            cycles += 1;

            cycles_usize = @intCast(usize, cycles);
            horiz_pos = @intCast(usize, @rem(cycles, 40));
            if (reg - 1 == horiz_pos) {
                screen[cycles_usize] = '#';
            } else if (reg == horiz_pos) {
                screen[cycles_usize] = '#';
            } else if (reg + 1 == horiz_pos) {
                screen[cycles_usize] = '#';
            }
            cycles += 1;

            reg += amt;
        } else if (std.mem.eql(u8, cmd, "noop")) {
            var cycles_usize = @intCast(usize, cycles);
            var horiz_pos = @intCast(usize, @rem(cycles, 40));
            if (reg - 1 == horiz_pos) {
                screen[cycles_usize] = '#';
            } else if (reg == horiz_pos) {
                screen[cycles_usize] = '#';
            } else if (reg + 1 == horiz_pos) {
                screen[cycles_usize] = '#';
            }
            cycles += 1;
        } else {
            std.debug.panic("unrecognized command: {s}", .{cmd});
        }
    }

    var row: u8 = 0;
    while (row < 6) {
        std.log.info("{s}", .{screen[(row * 40)..((row * 40) + 39)]});
        row += 1;
    }
}
