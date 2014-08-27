from llvm.core import Module, Constant, Type, Function, Builder

g_llvm_module = Module.new('CMCompiler')


class ASTNode:
    def __init__(self, context):
        self.context = context

    def print_ast(self):
        pass


class ProgramNode(ASTNode):
    def __init__(self, context, nodes):
        ASTNode.__init__(self, context)
        self.nodes = nodes

    def code_gen(self):
        for node in self.nodes:
            node.code_gen()


class VarDeclNode(ASTNode):
    def __init__(self, context, name, typo):
        ASTNode.__init__(self, context)
        self.name = name
        self.typo = typo

    def code_gen(self):
        pass


class ArrayDeclNode(ASTNode):
    def __init__(self, context, name, typo, length):
        ASTNode.__init__(self, context)
        self.name = name
        self.typo = typo
        self.length = length

    def code_gen(self):
        pass


class VarAssignNode(ASTNode):
    def __init__(self, context, name, expr_node):
        ASTNode.__init__(self, context)
        self.name = name
        self.expr_node = expr_node

    def code_gen(self):
        pass


class ArrayAssignNode(ASTNode):
    def __init__(self, context, name, index_expr_node, value_expr_node):
        ASTNode.__init__(self, context)
        self.name = name
        self.index_expr_node = index_expr_node
        self.value_expr_node = value_expr_node

    def code_gen(self):
        pass


class VarDeclAndAssignNode(ASTNode):
    def __init__(self, context, name, typo, expr_node):
        ASTNode.__init__(self, context)
        self.name = name
        self.typo = typo
        self.expr_node = expr_node

    def code_gen(self):
        pass


class ArrayDeclAndAssignNode(ASTNode):
    def __init__(self, context, name, typo, length, inital_value):
        ASTNode.__init__(self, context)
        self.name = name
        self.typo = typo
        self.length = length
        self.inital_value = inital_value

    def code_gen(self):
        pass


class BinaryExprNode(ASTNode):
    def __init__(self, context, left, operator, right):
        ASTNode.__init__(self, context)
        self.left = left
        self.operator = operator
        self.right = right

    def code_gen(self):
        pass


class FuncPrototypeNode(ASTNode):
    def __init__(self, context, name, ret_type, args):
        ASTNode.__init__(self, context)
        self.name = name
        self.ret_type = ret_type
        self.args = args

    def code_gen(self):
        return_type = Helper.get_type(self.ret_type)
        arg_types = [Helper.get_type(arg[1]) for arg in self.args if True]
        func_type = Type.function(return_type, arg_types, False)
        Function.new(g_llvm_module, func_type, self.name)


class FunctionNode(ASTNode):
    def __init__(self, context, prototype, body):
        ASTNode.__init__(self, context)
        self.prototype = prototype
        self.body = body

    def code_gen(self):
        pass


class IfElseNode(ASTNode):
    def __init__(self, context, if_node, else_node):
        ASTNode.__init__(self, context)
        self.if_node = if_node
        self.else_node = else_node

    def code_gen(self):
        pass


class IfNode(ASTNode):
    def __init__(self, context, stmts):
        ASTNode.__init__(self, context)
        self.stmts = stmts

    def code_gen(self):
        pass


class ElseNode(ASTNode):
    def __init__(self, context, stmts):
        ASTNode.__init__(self, context)
        self.stmts = stmts

    def code_gen(self):
        pass


class WhileNode(ASTNode):
    def __init__(self, context, expr, statements_node):
        ASTNode.__init__(self, context)
        self.expr = expr
        self.statements_node = statements_node

    def code_gen(self):
        pass


class ReturnNode(ASTNode):
    def __init__(self, context, expr_or_string_node):
        ASTNode.__init__(self, context)
        self.expr_or_string_node = expr_or_string_node

    def code_gen(self):
        pass


class ConstantNode(ASTNode):
    def __init__(self, context, value):
        ASTNode.__init__(self, context)
        self.value = value

    def code_gen(self):
        pass


class VariableNode(ASTNode):
    def __init__(self, context, name):
        ASTNode.__init__(self, context)
        self.name = name

    def code_gen(self):
        pass


class ArrayVariableNode(ASTNode):
    def __init__(self, context, name, index_expr_node):
        ASTNode.__init__(self, context)
        self.value = name
        self.index_expr_node = index_expr_node

    def code_gen(self):
        pass


class FunctionCallNode(ASTNode):
    def __init__(self, context, name, args):
        ASTNode.__init__(self, context)
        self.name = name
        self.args = args

    def code_gen(self):
        pass


class Helper:
    def __init__(self):
        pass

    @staticmethod
    def get_type(type_str):
        if type_str == 'int':
            return Type.int(32)
        elif type_str == 'double':
            return Type.double()
        elif type_str == 'String':
            ch = Type.int(8)
            return Type.pointer(ch)
        elif type_str == 'char':
            return Type.int(8)
        elif type_str == 'void':
            return Type.void()

