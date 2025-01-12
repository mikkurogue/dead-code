const std = @import("std");

const Allocator = std.mem.Allocator;

pub const TokenType = enum {
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
};

pub const LexerError = error{ UnterminatedBlockComment, UnterminatedDecorator };

pub const Token = struct {
    kind: TokenType,
    lexeme: []const u8,
    line: usize, // maybe rename this to row?
    col: usize,
};

pub const Lexer = struct {
    src: []const u8,
    index: usize,
    line: usize,
    col: usize,
    allocator: Allocator,

    pub fn init(allocator: Allocator, src: []const u8) Lexer {
        return Lexer{ .src = src, .index = 0, .line = 1, .col = 1, .allocator = allocator };
    }

    pub fn next_token(self: *Lexer) !Token {
        while (self.index < self.src.len) {
            const c = self.src[self.index];

            if (c == ' ' or c == '\t' or c == '\n') {
                self.advance();
                continue;
            }

            if (c == '/' and self.peek(1) == '/') {
                return self.scan_inline_comment();
            }

            if (c == '/' and self.peek(1) == '*') {
                return self.scan_block_comment();
            }

            break;
        }

        if (self.index >= self.src.len) {
            return Token{
                .kind = .EOF,
                .lexeme = "",
                .line = self.line,
                .col = self.col,
            };
        }

        const c = self.src[self.index];

        // check if the token is alphabetic
        // TODO: check if we need to use the isAlphanumeric instead
        if (std.ascii.isAlphabetic(c) or c == '_') {
            return self.scan_identifier_or_keyword();
        }

        // match strings
        if (c == '"' or c == '\'' or c == '¸') {
            return self.scan_string_literal();
        }

        // match the numbers
        if (std.ascii.isDigit(c)) {
            return self.scan_number_literal();
        }

        if (self.is_operator_or_separator(c)) {
            const start = self.index;
            self.advance();
            return Token{
                .kind = .Operator,
                .lexeme = self.src[start..self.index],
                .line = self.line,
                .col = self.col,
            };
        }

        // decorator match
        if (c == '@') {
            return self.scan_decorator();
        }

        // default:
        self.advance();
        return Token{ .kind = .Separator, .lexeme = "", .line = self.line, .col = self.col };
    }

    /// Advance to the next line or column
    /// if the identifier is a newLine, then move to next line and first column,
    /// if the identifier is not a new line then its just the next column
    fn advance(self: *Lexer) void {
        if (self.src[self.index] == '\n') {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        self.index += 1;
    }

    /// Peek the next identifier
    fn peek(self: *Lexer, offset: usize) ?u8 {
        const peek_index = self.index + offset;

        if (peek_index < self.src.len) {
            return self.src[peek_index];
        }

        // return null if out of bounds
        return null;
    }

    /// scan the identifier or keyword
    /// must allocate the result on the heap.
    /// can error
    fn scan_identifier_or_keyword(self: *Lexer) !Token {
        const start = self.index;

        while (self.index < self.src.len and (std.ascii.isAlphanumeric(self.src[self.index]) or self.src[self.index] == '_')) {
            self.advance();
        }

        const lexeme = self.src[start..self.index];
        const keyword = self.is_keyword(lexeme);

        return Token{
            .kind = if (keyword) .Keyword else .Identifier,
            .lexeme = try self.copy_lexeme(lexeme),
            .line = self.line,
            .col = self.col,
        };
    }

    /// scan the decorater
    fn scan_decorator(self: *Lexer) !Token {
        const start = self.index;
        self.advance();

        // each decorator must be folowed by an identifier or func call
        while (self.index < self.src.len and (std.ascii.isAlphanumeric(self.src[self.index]) or self.src[self.index] == '_' or self.src[self.index] == '.')) {
            self.advance();
        }

        if (self.index < self.src.len and self.src[self.index] == '(') {
            // func style decorators
            var pair_count: usize = 1;
            self.advance();
            while (self.index < self.src.len and pair_count > 0) {
                if (self.src[self.index] == '(') {
                    pair_count += 1;
                } else if (self.src[self.index] == ')') {
                    pair_count -= 1;
                }
                self.advance();
            }

            if (pair_count > 0) {
                return LexerError.UnterminatedDecorator;
            }
        }

        const lexeme = self.src[start..self.index];
        return Token{ .kind = .Decorator, .lexeme = try self.copy_lexeme(lexeme), .line = self.line, .col = self.col };
    }

    // scan the string literal
    fn scan_string_literal(self: *Lexer) !Token {
        const start = self.index;
        const quote_char = self.src[start];

        self.advance(); // skip opener quote.

        while (self.index < self.src.len and self.src[self.index] != quote_char) {
            self.advance();
        }

        self.advance(); // skip closer quote

        const lexeme = self.src[start..self.index];
        return Token{
            .kind = .StringLiteral,
            .lexeme = try self.copy_lexeme(lexeme),
            .line = self.line,
            .col = self.col,
        };
    }

    fn scan_number_literal(self: *Lexer) !Token {
        const start = self.index;
        var is_float = false;

        while (self.index < self.src.len) {
            const c = self.src[self.index];
            if (std.ascii.isDigit(c)) {
                self.advance();
            } else if (c == '.' and !is_float) {
                is_float = true;
                self.advance();
            } else {
                break;
            }
        }

        const lexeme = self.src[start..self.index];
        return Token{
            .kind = .NumberLiteral,
            .lexeme = try self.copy_lexeme(lexeme),
            .line = self.line,
            .col = self.col,
        };
    }

    fn is_number_literal(self: *Lexer, c: u8) bool {
        return std.ascii.isDigi(c) or (c == '.' and self.index + 1 < self.src.len and std.ascii.isDigit(self.src[self.index + 1]));
    }

    /// check if the lexeme is any of the keywords
    /// TODO: make the keyword array be complete
    fn is_keyword(self: *Lexer, lexeme: []const u8) bool {
        // ignore self, we only need it as a supported function of the struct.
        _ = self;
        const keywords = [_][]const u8{
            "function", "let", "const", "var", "class", "interface", "extends", "type", "enum",
        };

        for (keywords) |keyword| {
            if (std.mem.eql(u8, keyword, lexeme)) {
                return true;
            }
        }
        return false;
    }

    /// check if the character is an operator or a separator.
    /// an operator in this instance is for example the + symbol
    /// where as the separator is a . symbol
    fn is_operator_or_separator(self: *Lexer, c: u8) bool {
        _ = self;
        const operators = "+-*/=<>(){}[]:;,.";

        for (operators) |op| {
            if (op == c) {}
        }
        return false;
    }

    /// Scan the comment to find the end of the comment. Eventually
    /// we will just ignore comments.
    /// OR we have to add a scan for TODO, NOTE, FIXME tags.
    /// Maybe thats handy
    fn scan_inline_comment(self: *Lexer) !Token {
        const start = self.index;
        while (self.index < self.src.len and self.src[self.index] != '\n') {
            self.advance();
        }

        const lexeme = self.src[start..self.index];
        return Token{
            .kind = .Comment,
            .lexeme = try self.copy_lexeme(lexeme),
            .line = self.line,
            .col = self.col,
        };
    }

    /// Scan the block comment to find the end of the comment.
    /// See fn scan_inline_comment for the tag scanning
    fn scan_block_comment(self: *Lexer) !Token {
        const start = self.index;

        // Skip the initial /* tokens
        self.advance(); // skip '/'
        self.advance(); // skip '*'

        while (self.index < self.src.len) {
            const current_char = self.src[self.index];
            const next_char = self.peek(1);

            // Check for the block comment end '*/'
            if (current_char == '*' and next_char.? == '/') {
                self.advance(); // skip over '*'
                self.advance(); // skip over '/'
                break; // Block comment ends here
            }

            // If we encounter a newline, update line and column
            if (current_char == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }

            self.advance();
        }

        // If we reached the end without finding '*/', it's an unterminated comment
        if (self.index >= self.src.len) {
            std.log.warn("index: {any}", .{self.index});
            std.log.warn("src lenght: {any}", .{self.src.len});
            // return LexerError.UnterminatedBlockComment;
        }

        const lexeme = self.src[start..self.index];
        return Token{
            .kind = .Comment,
            .lexeme = try self.copy_lexeme(lexeme),
            .line = self.line,
            .col = self.col,
        };
    }
    fn copy_lexeme(self: *Lexer, lexeme: []const u8) ![]u8 {
        const buffer = try self.allocator.alloc(u8, lexeme.len);
        @memcpy(buffer, lexeme);
        return buffer;
    }
};

// TEST SECTION
test "lexer should tokenize inline comments" {
    const allocator = std.testing.allocator;

    const source = "// This is a comment\n";
    var lexer = Lexer.init(allocator, source);

    const token = try lexer.next_token();
    defer allocator.free(token.lexeme);

    try std.testing.expectEqual(TokenType.Comment, token.kind);
    try std.testing.expect(std.mem.eql(u8, token.lexeme, "// This is a comment"));
    try std.testing.expectEqual(1, token.line);
}

test "lexer shoud tokenize block comments" {
    const allocator = std.testing.allocator;
    const source = "/* this is a block comment */";
    var lexer = Lexer.init(allocator, source);

    const token = try lexer.next_token();
    defer allocator.free(token.lexeme);

    try std.testing.expectEqual(TokenType.Comment, token.kind);
    try std.testing.expect(std.mem.eql(u8, token.lexeme, source));
    try std.testing.expectEqual(1, token.line);
}
