const std = @import("std");

pub const SliceError = error{
    SliceTooSmall,
    NonWhitespaceInTrim,
};

pub fn all_respect(T: type, slice: []const T, predicate: fn (val: T) bool) bool {
    for (slice) |i| {
        if (!predicate(i))
            return false;
    }
    return true;
}

pub inline fn is_all_whitespace(slice: []const u8) bool {
    return all_respect(u8, slice, std.ascii.isWhitespace);
}

pub fn skip_n_whitespace(slice: []const u8, n: usize) ![]const u8 {
    if (slice.len < n)
        return SliceError.SliceTooSmall;
    if (!is_all_whitespace(slice[0..n]))
        return SliceError.NonWhitespaceInTrim;
    return slice[n..];
}
