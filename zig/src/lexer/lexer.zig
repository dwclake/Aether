const std = @import("std");
const token = @import("./token.zig");

pub const Lexer = struct {
    const Self = @This();

    input: []const u8,
    pos: usize = 0,
    read_pos: usize = 0,
    ch: u8 = 0,

    pub fn init(input: []const u8) Self {
        var l = Self{
            .input = input,
        };

        l.read_char();

        return l;
    }

    pub fn read_char(self: Self) void {
        if (self.read_pos >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_pos];
        }

        self.pos = self.read_pos;
        self.read_pos += 1;
    }
};

test "next token" {
    const input = "=+(){},;";

    const tests = [_]token.Token{ 
        .assign, 
        .plus, 
        .lparen, 
        .rparen, 
        .lsquirly, 
        .rsquirly, 
        .comma, 
        .semicolon 
    };

    var lex = Lexer.init(input);

    for (tests) |etok| {
        const tok = lex.next_token();

        try std.testing.expectEqualDeep(etok, tok);
    }
}
