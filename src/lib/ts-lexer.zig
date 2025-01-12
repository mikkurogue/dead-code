const std = @import("std");

const Allocator = std.mem.Allocator;

const TokenType = enum {
    Keyword,
    Identifier,
    StringLiteral,
    NumberLiteral,
    Operator,
    Separator,
    TypeAnnotation,
    Comment,
    Decorator,
    EOF,
    Error,
};

pub const Token = struct {
    kind: TokenType,
    lexeme: []const u8,
    line: usize,
    col: usize,
};

pub const Lexer = struct {
    src: []const u8,
    index: usize,
    line: usize,
    col: usize,
    allocator: Allocator,

    pub fn init(allocator: Allocator, src: []const u8) Lexer {
        return .{
            .src = src,
            .index = 0,
            .line = 1,
            .col = 1,
            .allocator = allocator,
        };
    }

    pub fn next_token(self: *Lexer) !Token {
        self.skip_whitespace();

        if (self.index >= self.src.len) {
            return Token{
                .kind = .EOF,
                .lexeme = "",
                .line = self.line,
                .col = self.col,
            };
        }

        const c = self.src[self.index];

        if (std.ascii.isAlphabetic(c) or c == '_') {
            return self.scan_identifier_or_keyword();
        }

        if (std.ascii.isDigit(c)) {
            return self.scan_number();
        }

        if (c == '"' or c == '\'') {
            return self.scan_string();
        }

        if (self.is_operator_or_separator(c)) {
            return self.scan_operator();
        }

        self.advance();
        return Token{
            .kind = .Error,
            .lexeme = "",
            .line = self.line,
            .col = self.col,
        };
    }

    fn skip_whitespace(self: *Lexer) void {
        while (self.index < self.src.len) {
            const c = self.src[self.index];
            switch (c) {
                ' ', '\t', '\n', '\r' => {
                    if (c == '\n') {
                        self.line += 1;
                        self.col = 1;
                    } else {
                        self.col += 1;
                    }
                    self.index += 1;
                },
                else => break,
            }
        }
    }
    fn advance(self: *Lexer) void {
        if (self.src[self.index] == '\n') {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        self.index += 1;
    }

    fn peek(self: *Lexer) ?u8 {
        if (self.index + 1 < self.src.len) {
            return self.src[self.index + 1];
        }
        return null;
    }

    fn scan_identifier_or_keyword(self: *Lexer) !Token {
        const start = self.index;
        const start_line = self.line;
        const start_col = self.col;

        while (self.index < self.src.len and
            (std.ascii.isAlphanumeric(self.src[self.index]) or self.src[self.index] == '_'))
        {
            self.advance();
        }

        const lexeme = self.src[start..self.index];
        const token_type = if (self.is_keyword(lexeme)) TokenType.Keyword else TokenType.Identifier;

        return Token{
            .kind = token_type,
            .lexeme = try self.copy_lexeme(lexeme),
            .line = start_line,
            .col = start_col,
        };
    }

    fn scan_number(self: *Lexer) !Token {
        const start = self.index;
        const start_line = self.line;
        const start_col = self.col;

        while (self.index < self.src.len and std.ascii.isDigit(self.src[self.index])) {
            self.advance();
        }

        if (self.index < self.src.len and self.src[self.index] == '.') {
            self.advance();
            while (self.index < self.src.len and std.ascii.isDigit(self.src[self.index])) {
                self.advance();
            }
        }

        return Token{
            .kind = .NumberLiteral,
            .lexeme = try self.copy_lexeme(self.src[start..self.index]),
            .line = start_line,
            .col = start_col,
        };
    }

    fn scan_string(self: *Lexer) !Token {
        const start = self.index;
        const start_line = self.line;
        const start_col = self.col;
        const quote = self.src[self.index];

        self.advance(); // Skip opening quote
        while (self.index < self.src.len and self.src[self.index] != quote) {
            self.advance();
        }
        if (self.index < self.src.len) self.advance(); // Skip closing quote

        return Token{
            .kind = .StringLiteral,
            .lexeme = try self.copy_lexeme(self.src[start..self.index]),
            .line = start_line,
            .col = start_col,
        };
    }

    fn scan_operator(self: *Lexer) !Token {
        const start = self.index;
        const start_line = self.line;
        const start_col = self.col;

        self.advance();
        if (self.index < self.src.len and self.is_operator_or_separator(self.src[self.index])) {
            self.advance();
        }

        return Token{
            .kind = .Operator,
            .lexeme = try self.copy_lexeme(self.src[start..self.index]),
            .line = start_line,
            .col = start_col,
        };
    }

    fn is_keyword(self: *Lexer, lexeme: []const u8) bool {
        _ = self;
        const keywords = [_][]const u8{
            "let",
            "const",
            "var",
            "function",
            "return",
            "if",
            "else",
            "for",
            "while",
        };

        for (keywords) |keyword| {
            if (std.mem.eql(u8, keyword, lexeme)) {
                return true;
            }
        }
        return false;
    }

    fn is_operator_or_separator(self: *Lexer, c: u8) bool {
        _ = self;
        return switch (c) {
            '+',
            '-',
            '*',
            '/',
            '=',
            '<',
            '>',
            '!',
            '(',
            ')',
            '{',
            '}',
            '[',
            ']',
            ':',
            ';',
            ',',
            '.',
            => true,
            else => false,
        };
    }

    fn copy_lexeme(self: *Lexer, lexeme: []const u8) ![]u8 {
        const buf = try self.allocator.alloc(u8, lexeme.len);
        @memcpy(buf, lexeme);
        return buf;
    }
};

// TEST SECTION
// test "lexer should tokenize inline comments" {
//     const allocator = std.testing.allocator;
//
//     const source = "// This is a comment\n";
//     var lexer = Lexer.init(allocator, source);
//
//     const token = try lexer.next_token();
//     defer allocator.free(token.lexeme);
//
//     try std.testing.expectEqual(TokenType.Comment, token.kind);
//     try std.testing.expect(std.mem.eql(u8, token.lexeme, source));
//     try std.testing.expectEqual(1, token.line);
// }
//
// test "lexer shoud tokenize block comments" {
//     const allocator = std.testing.allocator;
//     const source = "/* this is a block comment */";
//     var lexer = Lexer.init(allocator, source);
//
//     const token = try lexer.next_token();
//     defer allocator.free(token.lexeme);
//
//     try std.testing.expectEqual(TokenType.Comment, token.kind);
//     try std.testing.expect(std.mem.eql(u8, token.lexeme, source));
//     try std.testing.expectEqual(1, token.line);
// }
//
// test "keyword" {
//     const allocator = std.testing.allocator;
//     const source = "function myFunc() { const x = \"Hello World\" }";
//
//     var lexer = Lexer.init(allocator, source);
//     var index: usize = 0;
//     const token = try lexer.next_token();
//     while (source.len > index) : (index += 1) {
//         try std.testing.expectEqual(TokenType.Keyword, token.kind);
//         try std.testing.expect(std.mem.eql(u8, token.lexeme, "function"));
//     }
//     defer lexer.allocator.free(token.lexeme);
// }
