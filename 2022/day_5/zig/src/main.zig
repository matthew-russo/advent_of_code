const std = @import("std");
const StringLinkedList = std.SinglyLinkedList([]const u8);
const CharLinkedList = std.SinglyLinkedList(u8);

const State = enum {
    building_stacks,
    bypass_empty_line,
    reading_commands,
};

pub fn main() !void {
    var allocator_provider = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = allocator_provider.allocator();

    var file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var stack_lines = StringLinkedList{};
    var stacks = std.ArrayList(CharLinkedList).init(allocator);
    var staging = CharLinkedList{};

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var current_state = State.building_stacks;

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        switch (current_state) {
            State.building_stacks => {
                if (std.mem.containsAtLeast(u8, line, 1, "[")) {
                    // we're still in stack mode.
                    var node = try allocator.create(StringLinkedList.Node);
                    var alloced_line = try allocator.alloc(u8, line.len);

                    std.mem.copy(u8, alloced_line, line);
                    node.* = StringLinkedList.Node{ .data = alloced_line };

                    stack_lines.prepend(node);
                } else {
                    var spliterator = std.mem.tokenize(u8, line, " ");
                    while (spliterator.next()) |_| {
                        try stacks.append(CharLinkedList{});
                    }

                    outer: while (stack_lines.popFirst()) |stack_line_node| {
                        var col: u8 = 0;
                        inner: while (col < stacks.items.len) {
                            var idx = 1 + (col * 4);
                            if (stack_line_node.data.len <= idx) {
                                continue :outer;
                            }
                            var data = stack_line_node.data[idx];
                            if (data == ' ') {
                                col += 1;
                                continue :inner;
                            }
                            var node = try allocator.create(CharLinkedList.Node);
                            node.* = CharLinkedList.Node{ .data = stack_line_node.data[idx] };
                            stacks.items[col].prepend(node);
                            col += 1;
                        }
                    }

                    current_state = State.bypass_empty_line;
                }
            },
            State.bypass_empty_line => {
                current_state = State.reading_commands;
            },
            State.reading_commands => {
                var spliterator = std.mem.tokenize(u8, line, " ");
                _ = spliterator.next() orelse unreachable; // move
                var countStr = spliterator.next() orelse unreachable;
                var count = try std.fmt.parseInt(u16, countStr, 10);
                _ = spliterator.next() orelse unreachable; // from
                var srcStr = spliterator.next() orelse unreachable;
                var src = try std.fmt.parseInt(u8, srcStr, 10);
                _ = spliterator.next() orelse unreachable; // to
                var dstStr = spliterator.next() orelse unreachable;
                var dst = try std.fmt.parseInt(u8, dstStr, 10);

                var curr: u16 = 0;
                while (curr < count) {
                    var node = stacks.items[src - 1].popFirst() orelse unreachable;
                    staging.prepend(node);
                    curr += 1;
                }

                curr = 0;
                while (curr < count) {
                    var node = staging.popFirst() orelse unreachable;
                    stacks.items[dst - 1].prepend(node);
                    curr += 1;
                }
            },
        }
    }

    var idx: u8 = 0;
    while (idx < stacks.items.len) {
        var top = stacks.items[idx].popFirst() orelse unreachable;
        std.log.info("top of stack {d}: {c}", .{ idx + 1, top.data });
        idx += 1;
    }
}
