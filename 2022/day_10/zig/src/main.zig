const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var cycles: i64 = 0;
    var reg: i64 = 1;

    var acc: i64 = 0;

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (cycles > 220) {
            break;
        }

        var spliterator = std.mem.tokenize(u8, line, " ");
        var cmd = spliterator.next() orelse unreachable;

        if (std.mem.eql(u8, cmd, "addx")) {
            var amt_str = spliterator.next() orelse unreachable;
            var amt = std.fmt.parseInt(i64, amt_str, 10) catch unreachable;

            cycles += 1;
            if (@rem(cycles - 20, 40) == 0) {
                std.log.info("cycles: {d}, reg {d}", .{ cycles, reg });
                acc += (reg * cycles);
            }

            cycles += 1;
            if (@rem(cycles - 20, 40) == 0) {
                std.log.info("cycles: {d}, reg {d}", .{ cycles, reg });
                acc += (reg * cycles);
            }

            reg += amt;
        } else if (std.mem.eql(u8, cmd, "noop")) {
            cycles += 1;
            if (@rem(cycles - 20, 40) == 0) {
                std.log.info("cycles: {d}, reg {d}", .{ cycles, reg });
                acc += (reg * cycles);
            }
        } else {
            std.debug.panic("unrecognized command: {s}", .{cmd});
        }
    }

    std.log.info("acc is {d}", .{acc});
}
