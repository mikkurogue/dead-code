const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Node = union(enum) {
    Program: struct {
        statements: []*Node,
    },
    VarDecl: struct {
        name: *Node,
        initialiser: *Node,
        type_annotation: ?*Node,
    },
    FuncDecl: struct {
        name: *Node,
        params: []*Node,
        return_type: ?*Node,
        body: *Node,
    },
    ExpressionStatement: struct {
        expression: *Node,
    },
    IfStatement: struct {
        condition: *Node,
        consequence: *Node,
        alternative: ?*Node,
    },
    ForStatement: struct {
        init: *Node,
        condition: *Node,
        increment: *Node,
        body: *Node,
    },
    ReturnStatement: struct {
        expression: *Node,
    },
    BinaryExpression: struct {
        left: *Node,
        operator: Operator,
        right: *Node,
    },
    Identifier: struct {
        name: []const u8,
    },
    Literal: union(enum) {
        Number: f64,
        String: []const u8,
        Boolean: bool,
        Null: void,
        Undefined: void,
    },
    TypeAnnotation: struct {
        type_name: []const u8,
    },
    Block: struct {
        statements: []*Node,
    },
    Parameter: struct {
        name: *Node,
        type_annotation: ?*Node,
    },

    pub fn deinit(self: *Node, allocator: Allocator) void {
        switch (self.*) {
            .Program => |*prog| {
                for (prog.statements) |stmt| {
                    stmt.deinit(allocator);
                    allocator.destroy(stmt);
                }
                allocator.free(prog.statements);
            },
            .VarDecl => |*decl| {
                decl.name.deinit(allocator);
                allocator.destroy(decl.name);
                decl.initialiser.deinit(allocator);
                allocator.destroy(decl.initialiser);
                if (decl.type_annotation) |annotation| {
                    annotation.deinit(allocator);
                    allocator.destroy(annotation);
                }
            },
            .Identifier => |ident| {
                allocator.free(ident.name);
            },
            .Literal => |*lit| {
                switch (lit.*) {
                    .String => |str| allocator.free(str),
                    else => {},
                }
            },
            .TypeAnnotation => |annotation| {
                allocator.free(annotation.type_name);
            },
            .Block => |*block| {
                for (block.statements) |stmt| {
                    stmt.deinit(allocator);
                    allocator.destroy(stmt);
                }
                allocator.free(block.statements);
            },
            else => {},
        }
    }
};

pub const Operator = enum {
    Plus,
    Minus,
    Multiply,
    Divide,
    Equals,
    GreaterThan,
    LessThan,
    NotEqual,
    Assignment,
};
