pub fn hashBytes(value: []const u8) u64 {
    var h: u64 = 14695981039346656037;
    for (value) |ch| {
        h ^= ch;
        h *%= 1099511628211;
    }
    return h;
}
