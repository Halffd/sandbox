const std = @import("std");

fn somavet(v: []const i32, n: usize) i32 {
    if (n == 0) {
        return 0;
    } else {
        return v[n - 1] + somavet(v, n - 1);
    }
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();

    const sizes = [_]usize{100_000, 200_000, 300_000};

    var prng = std.rand.DefaultPrng.init();
    var random = prng.random();

    for (sizes) |size| {
        const array = try allocator.alloc(i32, size);
        defer allocator.free(array);

        for (array) |*value| {
            value.* = @intCast(i32, random.intRangeAtMost(i32, 0, 99));
        }

        const sum = somavet(array, size);
        try stdout.print("Sum of elements in vector of size {}: {}\n", .{size, sum});
    }
}
