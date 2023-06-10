const std = @import("std");

pub const Token = union(enum) {
  illegal,

  fun,
  let,

  ident: []const u8,
  int: []const u8,

  assign,
  plus,

  comma,
  semicolon,

  lparen,
  rparen,
  lsquirly,
  rsquirly
};
