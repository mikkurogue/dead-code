const std = @import("std");
const Lexer = @import("ts-lexer.zig").Lexer;
const Token = @import("ts-lexer.zig").Token;
const TokenType = @import("ts-lexer.zig").TokenType;
const AST = @import("ts-ast.zig");
const Node = AST.Node;
const Allocator = std.mem.Allocator;

pub const Parser = struct {
    lexer: *Lexer,
    current_token: Token,
    allocator: Allocator,

    pub fn init(allocator: Allocator, lexer: *Lexer) !Parser {
        const first_token = try lexer.next_token();
        return Parser{
            .allocator = allocator,
            .lexer = lexer,
            .current_token = first_token,
        };
    }

    pub fn deinit(self: *Parser) void {
        if (self.current_token.lexeme.len > 0) {
            self.allocator.free(self.current_token.lexeme);
        }
    }

    fn advance(self: *Parser) !void {
        if (self.current_token.lexeme.len > 0) {
            self.allocator.free(self.current_token.lexeme);
        }
        self.current_token = try self.lexer.next_token();
    }

    pub fn parse_program(self: *Parser) !*Node {
        var statements = std.ArrayList(*Node).init(self.allocator);
        errdefer {
            for (statements.items) |stmt| {
                stmt.deinit(self.allocator);
                self.allocator.destroy(stmt);
            }
            statements.deinit();
        }

        while (self.current_token.kind != .EOF) {
            const stmt = try self.parse_statement();
            try statements.append(stmt);
        }

        const program = try self.allocator.create(Node);
        errdefer self.allocator.destroy(program);

        program.* = Node{ .Program = .{
            .statements = try statements.toOwnedSlice(),
        } };
        return program;
    }

    fn parse_statement(self: *Parser) !*Node {
        return switch (self.current_token.kind) {
            .Keyword => {
                if (std.mem.eql(u8, self.current_token.lexeme, "let")) {
                    return self.parse_var_decl();
                } else {
                    return error.UnexpectedKeyword;
                }
            },
            else => error.UnexpectedToken,
        };
    }

    fn parse_var_decl(self: *Parser) !*Node {
        try self.advance(); // skip 'let'

        if (self.current_token.kind != .Identifier) {
            return error.ExpectedIdentifier;
        }

        const name_node = try self.allocator.create(Node);
        errdefer self.allocator.destroy(name_node);

        name_node.* = Node{ .Identifier = .{
            .name = try self.allocator.dupe(u8, self.current_token.lexeme),
        } };

        try self.advance(); // skip identifier

        if (self.current_token.kind != .Operator or
            !std.mem.eql(u8, self.current_token.lexeme, "="))
        {
            return error.ExpectedEquals;
        }

        try self.advance(); // skip '='

        const init_node = try self.parse_expression();
        errdefer {
            init_node.deinit(self.allocator);
            self.allocator.destroy(init_node);
        }

        const var_decl = try self.allocator.create(Node);
        errdefer self.allocator.destroy(var_decl);

        var_decl.* = Node{ .VarDecl = .{
            .name = name_node,
            .initialiser = init_node,
            .type_annotation = null,
        } };

        return var_decl;
    }

    fn parse_expression(self: *Parser) !*Node {
        const node = try self.allocator.create(Node);
        errdefer self.allocator.destroy(node);

        node.* = switch (self.current_token.kind) {
            .NumberLiteral => Node{
                .Literal = .{
                    .Number = try std.fmt.parseFloat(f64, self.current_token.lexeme),
                },
            },
            .StringLiteral => Node{
                .Literal = .{
                    .String = try self.allocator.dupe(u8, self.current_token.lexeme),
                },
            },
            .Identifier => Node{
                .Identifier = .{
                    .name = try self.allocator.dupe(u8, self.current_token.lexeme),
                },
            },
            else => return error.UnexpectedToken,
        };

        try self.advance();
        return node;
    }
};

test "lexer and parser integration - simple variable declaration" {
    const allocator = std.testing.allocator;
    const source = "let x = 42";

    var lexer = Lexer.init(allocator, source);
    var parser = try Parser.init(allocator, &lexer);

    const program = try parser.parse_program();

    // Test program structure
    try std.testing.expect(program.Program.statements.len == 1);

    // Test variable declaration
    const var_decl = program.Program.statements[0];
    try std.testing.expect(@as(std.meta.Tag(Node), var_decl.*) == .VarDecl);

    // Test variable name
    const name_node = var_decl.VarDecl.name;
    try std.testing.expect(@as(std.meta.Tag(Node), name_node.*) == .Identifier);
    try std.testing.expectEqualStrings("x", name_node.Identifier.name);

    // Test initializer value
    const init_node = var_decl.VarDecl.initialiser;
    try std.testing.expect(@as(std.meta.Tag(Node), init_node.*) == .Literal);
    try std.testing.expect(init_node.Literal == .Number);
    try std.testing.expect(init_node.Literal.Number == 42);

    std.log.warn("name: {s}", .{name_node.Identifier.name});

    std.log.warn("val: {d}", .{init_node.Literal.Number});

    defer parser.deinit();
    defer allocator.destroy(program);
    defer program.deinit(allocator);
}
