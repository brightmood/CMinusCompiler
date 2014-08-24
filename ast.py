class ASTNode:
    def __init__(self, context):
        self.context = context

    def print_ast(self):
        pass


class ProgramNode(ASTNode):
    def __init__(self, context, nodes):
        ASTNode.__init__(self, context)
        self.nodes = nodes


class VarDeclNode(ASTNode):
    def __init__(self, context, name, typo):
        ASTNode.__init__(self, context)
        self.name = name
        self.typo = typo


class ArrayDeclNode(ASTNode):
    def __init__(self, context, name, typo, length):
        ASTNode.__init__(self, context)
        self.name = name
        self.typo = typo
        self.length = length


class VarAssignNode(ASTNode):
    def __init__(self, context, name, expr_node):
        ASTNode.__init__(self, context)
        self.name = name
        self.expr_node = expr_node


class ArrayAssignNode(ASTNode):
    def __init__(self, context, name, index_expr_node, value_expr_node):
        ASTNode.__init__(self, context)
        self.name = name
        self.index_expr_node = index_expr_node
        self.value_expr_node = value_expr_node


class VarDeclAndAssignNode(ASTNode):
    def __init__(self, context, name, typo, expr_node):
        ASTNode.__init__(self, context)
        self.name = name
        self.typo = typo
        self.expr_node = expr_node


class ArrayDeclAndAssignNode(ASTNode):
    def __init__(self, context, name, typo, length, inital_value):
        ASTNode.__init__(self, context)
        self.name = name
        self.typo = typo
        self.length = length
        self.inital_value = inital_value


class BinaryExprNode(ASTNode):
    def __init__(self, context, left, operator, right):
        ASTNode.__init__(self, context)
        self.left = left
        self.operator = operator
        self.right = right


class FuncPrototypeNode(ASTNode):
    def __init__(self, context, name, ret_type, args):
        ASTNode.__init__(self, context)
        self.name = name
        self.ret_type = ret_type
        self.args = args


class FunctionNode(ASTNode):
    def __init__(self, context, prototype, body):
        ASTNode.__init__(self, context)
        self.prototype = prototype
        self.body = body


class IfElseNode(ASTNode):
    def __init__(self, context, if_node, else_node):
        ASTNode.__init__(self, context)
        self.if_node = if_node
        self.else_node = else_node


class IfNode(ASTNode):
    def __init__(self, context, stmts):
        ASTNode.__init__(self, context)
        self.stmts = stmts


class ElseNode(ASTNode):
    def __init__(self, context, stmts):
        ASTNode.__init__(self, context)
        self.stmts = stmts


class WhileNode(ASTNode):
    def __init__(self, context, expr, statements_node):
        ASTNode.__init__(self, context)
        self.expr = expr
        self.statements_node = statements_node


class ReturnNode(ASTNode):
    def __init__(self, context, expr_or_string_node):
        ASTNode.__init__(self, context)
        self.expr_or_string_node = expr_or_string_node


class ConstantNode(ASTNode):
    def __init__(self, context, value):
        ASTNode.__init__(self, context)
        self.value = value


class VariableNode(ASTNode):
    def __init__(self, context, name):
        ASTNode.__init__(self, context)
        self.name = name


class ArrayVariableNode(ASTNode):
    def __init__(self, context, name, index_expr_node):
        ASTNode.__init__(self, context)
        self.value = name
        self.index_expr_node = index_expr_node


class FunctionCallNode(ASTNode):
    def __init__(self, context, name, args):
        ASTNode.__init__(self, context)
        self.name = name
        self.args = args


