const std = @import("std");
const DirMap = std.AutoHashMap(u64, Directory);
const CharLinkedList = std.SinglyLinkedList(u8);

const Directory = struct {
    parent_id: u64,
    id: u64,
    children: std.StringHashMap(u64),
    immediate_size: u64,
};

fn alloc_id(i: *u64) u64 {
    var temp = i.*;
    i.* += 1;
    return temp;
}

pub fn main() !void {
    var allocator_provider = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = allocator_provider.allocator();

    var file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var dir_increment: u64 = 0;

    var all_dirs = DirMap.init(allocator);
    const root_id = alloc_id(&dir_increment);
    var root = Directory{
        .id = root_id,
        .parent_id = std.math.maxInt(u64),
        .children = std.StringHashMap(u64).init(allocator),
        .immediate_size = 0,
    };
    all_dirs.put(root.id, root) catch unreachable;

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var curr_dir_id = root_id;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line[0] == '$') {
            // processing command
            var spliterator = std.mem.tokenize(u8, line, " ");
            var dollar = spliterator.next() orelse unreachable;
            std.debug.assert(std.mem.eql(u8, dollar, "$")); // compares for string content, not for address equality
            var cmd = spliterator.next() orelse unreachable;
            if (std.mem.eql(u8, cmd, "cd")) {
                var arg = spliterator.next() orelse unreachable;
                if (std.mem.eql(u8, arg, "..")) {
                    var curr_dir = all_dirs.getPtr(curr_dir_id) orelse unreachable;
                    curr_dir_id = curr_dir.*.parent_id;
                } else if (std.mem.eql(u8, arg, "/")) {
                    curr_dir_id = root_id;
                } else {
                    var curr_dir = all_dirs.getPtr(curr_dir_id) orelse unreachable;
                    curr_dir_id = curr_dir.*.children.get(arg) orelse unreachable;
                }
            } else if (std.mem.eql(u8, cmd, "ls")) {
                // nothing to do. next `n` lines will process the result
            } else {
                std.debug.panic("Unknown cmd: {s}", .{cmd});
            }
        } else {
            // processing `ls` results
            var curr_dir = all_dirs.getPtr(curr_dir_id) orelse unreachable;

            var spliterator = std.mem.tokenize(u8, line, " ");
            var size_or_dir_str = spliterator.next() orelse unreachable;

            if (std.mem.eql(u8, size_or_dir_str, "dir")) {
                var id = alloc_id(&dir_increment);
                var new_dir = Directory{
                    .id = id,
                    .parent_id = curr_dir_id,
                    .children = std.StringHashMap(u64).init(allocator),
                    .immediate_size = 0,
                };
                var child_name_borrow = spliterator.next() orelse unreachable;
                var child_name = allocator.alloc(u8, child_name_borrow.len) catch unreachable;
                std.mem.copy(u8, child_name, child_name_borrow);
                curr_dir.*.children.put(child_name, id) catch unreachable;
                all_dirs.put(id, new_dir) catch unreachable;
            } else {
                var size = std.fmt.parseInt(u64, size_or_dir_str, 10) catch unreachable;
                curr_dir.*.immediate_size += size;
            }
        }
    }

    var accumulator: u64 = 0;
    _ = accumulateDirSizes(&all_dirs, root_id, &accumulator);
    std.log.info("Sum of small dirs: {d}", .{accumulator});
}

fn accumulateDirSizes(all_dirs: *DirMap, dir_id: u64, accumulator: *u64) u64 {
    var curr_dir = all_dirs.*.getPtr(dir_id) orelse unreachable;
    var total_size: u64 = curr_dir.*.immediate_size;
    var child_iterator = curr_dir.*.children.iterator();
    while (child_iterator.next()) |child| {
        total_size += accumulateDirSizes(all_dirs, child.value_ptr.*, accumulator);
    }
    if (total_size <= 100000) {
        accumulator.* += total_size;
    }
    return total_size;
}
