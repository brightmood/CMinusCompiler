from llvm.core import Module, Constant, Type, Function, Builder, RPRED_ULT, GlobalVariable, ConstantArray
import cmexception
from lexer import LexmeType

g_llvm_module = Module.new('CMCompiler')
g_llvm_builder = None
func_table = {}
constant_string_num = 0


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
        return g_llvm_module


class VarDeclNode(ASTNode):
    def __init__(self, context, var_name_token, typo):
        ASTNode.__init__(self, context)
        self.var_name_token = var_name_token
        self.typo = typo

    def code_gen(self):
        if self.context.parent_context is None:
            if self.var_name_token.word in self.context.type_table:
                raise cmexception.RedefineException(self.var_name_token, 'Global Variable')
            else:
                t = Helper.get_type(self.typo.word)
                gv = GlobalVariable.new(g_llvm_module, t, self.var_name_token.word)
                self.context.type_table[self.var_name_token.word] = t
                if self.typo.word == 'int':
                    gv.initializer = Constant.int(Type.int(32), 0)
                elif self.typo.word == 'double':
                    gv.initializer = Constant.real(Type.double(), 0)
                elif self.typo.word == 'char':
                    gv.initializer = Constant.int(Type.int(8), 0)
                else:
                    gv.initializer = Constant.stringz("")
        else:
            if not self.var_name_token.word in self.context.type_table:
                t = Helper.get_type(self.typo.word)
                var_addr = g_llvm_builder.alloca(t)
                self.context.type_table[self.var_name_token.word] = t
                self.context.value_table[self.var_name_token.word] = var_addr
            else:
                raise cmexception.RedefineException(self.var_name_token)


class ArrayDeclNode(ASTNode):
    def __init__(self, context, array_name_token, typo, length_token):
        ASTNode.__init__(self, context)
        self.array_name_token = array_name_token
        self.typo = typo
        self.length_token = length_token

    def code_gen(self):
        length = int(self.length_token.word)
        if length < 0 or length > (1 << 31) - 1:
            raise cmexception.SyntaxException(self.length_token, ['<a integer between 0 and 2e31-1>'])
        if self.context.parent_context is None:
            if self.array_name_token.word in self.context.type_table:
                raise cmexception.RedefineException(self.array_name_token, 'Global Variable')
            else:
                t = Helper.get_array_type(self.typo.word, length)
                gv = GlobalVariable.new(g_llvm_module, t, self.array_name_token.word)
                self.context.type_table[self.array_name_token.word] = t
        else:
            if not self.array_name_token.word in self.context.type_table:
                t = Helper.get_array_type(self.typo.word, length)
                var_addr = g_llvm_builder.alloca(t)
                self.context.type_table[self.array_name_token.word] = t
                self.context.value_table[self.array_name_token.word] = var_addr
            else:
                raise cmexception.RedefineException(self.array_name_token)


class VarAssignNode(ASTNode):
    def __init__(self, context, var_name_token, expr_node):
        ASTNode.__init__(self, context)
        self.var_name_token = var_name_token
        self.expr_node = expr_node

    def code_gen(self):
        if self.var_name_token.word in self.context.type_table:
            # var_type = self.context.type_table[self.var_name_token.word]
            value_addr = self.context.value_table[self.var_name_token.word]
            expr = self.expr_node.code_gen()
            if expr in g_llvm_module.global_variables:
                expr = g_llvm_builder.gep(expr, [Constant.int(Type.int(32), 0), Constant.int(Type.int(32), 0)])
            g_llvm_builder.store(expr, value_addr)
        else:
            if self.var_name_token.word in self.context.parent_context.type_table:
                gv = g_llvm_module.get_global_variable_named(self.var_name_token.word)
                expr = self.expr_node.code_gen()
                g_llvm_builder.store(expr, gv)
            else:
                raise cmexception.NotDefinedException(self.var_name_token, 'Variable')


class ArrayAssignNode(ASTNode):
    def __init__(self, context, array_name_token, index_expr_node, value_expr_node):
        ASTNode.__init__(self, context)
        self.array_name_token = array_name_token
        self.index_expr_node = index_expr_node
        self.value_expr_node = value_expr_node

    def code_gen(self):
        index = self.index_expr_node.code_gen()
        expr = self.value_expr_node.code_gen()
        if self.array_name_token.word in self.context.type_table:
            t = self.context.type_table[self.array_name_token.word]
            if t.count <= index.z_ext_value:
                raise cmexception.ArrayIndexOutOfBoundException(self.index_expr_node.constant_token, t.count)
            value_addr = self.context.value_table[self.array_name_token.word]
            addr = g_llvm_builder.gep(value_addr, [Constant.int(Type.int(32), 0), index])
            g_llvm_builder.store(expr, addr)
        else:
            if self.array_name_token.word in self.context.parent_context.type_table:
                t = self.context.parent_context.type_table[self.array_name_token.word]
                if t.count <= index.z_ext_value:
                    raise cmexception.ArrayIndexOutOfBoundException(self.index_expr_node.constant_token, t.count)
                global_array = g_llvm_module.get_global_variable_named(self.array_name_token.word)
                addr = g_llvm_builder.gep(global_array, [Constant.int(Type.int(32), 0), index])
                g_llvm_builder.store(expr, addr)
            else:
                raise cmexception.NotDefinedException(self.array_name_token, 'Array')


class VarDeclAndAssignNode(ASTNode):
    def __init__(self, context, var_name_token, typo, expr_node):
        ASTNode.__init__(self, context)
        self.var_name_token = var_name_token
        self.typo = typo
        self.expr_node = expr_node

    def code_gen(self):
        if self.context.parent_context is None:
            if self.var_name_token.word in self.context.type_table:
                raise cmexception.RedefineException(self.var_name_token, 'Global Variable')
            else:
                t = Helper.get_type(self.typo.word)
                expr = self.expr_node.code_gen()
                if expr in g_llvm_module.global_variables:
                    expr.name = self.var_name_token.word
                else:
                    gv = GlobalVariable.new(g_llvm_module, t, self.var_name_token.word)
                    gv.initializer = expr
                self.context.type_table[self.var_name_token.word] = t
        else:
            if not self.var_name_token.word in self.context.type_table:
                t = Helper.get_type(self.typo.word)
                var_addr = g_llvm_builder.alloca(t)
                self.context.type_table[self.var_name_token.word] = t
                self.context.value_table[self.var_name_token.word] = var_addr
                expr = self.expr_node.code_gen()
                if expr in g_llvm_module.global_variables:
                    expr = g_llvm_builder.gep(expr, [Constant.int(Type.int(32), 0), Constant.int(Type.int(32), 0)])
                g_llvm_builder.store(expr, var_addr)
            else:
                raise cmexception.RedefineException(self.var_name_token)


class ArrayDeclAndAssignNode(ASTNode):
    def __init__(self, context, array_name_token, typo, length, initial_value):
        ASTNode.__init__(self, context)
        self.array_name_token = array_name_token
        self.typo = typo
        self.length = length
        self.initial_value = initial_value

    def code_gen(self):
        if self.context.parent_context is None:
            if self.array_name_token.word in self.context.type_table:
                raise cmexception.RedefineException(self.array_name_token)
            else:
                t = Helper.get_array_type(self.typo.word, int(self.length.word))
                gv = GlobalVariable.new(g_llvm_module, t, self.array_name_token.word)
                initials = [i.code_gen() for i in self.initial_value if True]
                constant_array = ConstantArray.array(Helper.get_type(self.typo.word), initials)
                gv.initializer = constant_array
                self.context.type_table[self.array_name_token.word] = t
        else:
            if self.array_name_token.word in self.context.type_table:
                raise cmexception.RedefineException(self.array_name_token)
            else:
                t = Helper.get_array_type(self.typo.word, int(self.length.word))
                array_addr = g_llvm_builder.alloca(t)
                inx = 0
                for i in self.initial_value:
                    value = i.code_gen()
                    if value in g_llvm_module.global_variables:
                        string_value_ptr = g_llvm_builder.gep(value, [Constant.int(Type.int(32), 0), Constant.int(Type.int(32), 0)])
                        var_addr = g_llvm_builder.gep(array_addr, [Constant.int(Type.int(32), 0), Constant.int(Type.int(32), inx)])
                        g_llvm_builder.store(string_value_ptr, var_addr)
                    else:
                        var_addr = g_llvm_builder.gep(array_addr, [Constant.int(Type.int(32), 0), Constant.int(Type.int(32), inx)])
                        g_llvm_builder.store(value, var_addr)
                    inx += 1
                self.context.type_table[self.array_name_token.word] = t


def choose_method(left, op, right):
    method_table = {
        'i':
            {
                '+': 'add', '-': 'sub', '*': 'mul', '/': 'div',
                '%': 'srem', '<<': 'shl', '>>': 'lshr'
            },
        'f':
            {
                '+': 'fadd', '-': 'fsub', '*': 'fmul', '/': 'fdiv'
            }
    }

    if left.type == Type.double() or right.type == Type.double():
        return method_table['f'][op]
    return method_table['i'][op]


class BinaryExprNode(ASTNode):
    def __init__(self, context, left, operator, right):
        ASTNode.__init__(self, context)
        self.left = left
        self.operator = operator
        self.right = right

    def code_gen(self):
        op = self.operator.word
        left = self.left.code_gen()
        right = self.right.code_gen()
        method = choose_method(left, op, right)
        if g_llvm_builder:
            if method[0] == 'f':
                if left.type == Type.int(32):
                    left = g_llvm_builder.sitofp(left, Type.double())
                if right.type == Type.int(32):
                    right = g_llvm_builder.sitofp(right, Type.double())
            else:
                if left.type == Type.int(8):
                    left = g_llvm_builder.sext(left, Type.int(32))
                if right.type == Type.int(8):
                    right = g_llvm_builder.sext(right, Type.int(32))
            return getattr(g_llvm_builder, method)(left, right)
        else:
            if method[0] == 'f':
                if left.type == Type.int(32):
                    left = left.sitofp(Type.double())
                if right.type == Type.int(32):
                    right = right.sitofp(Type.double())
            else:
                if left.type == Type.int(8):
                    left = left.sext(Type.int(32))
                if right.type == Type.int(8):
                    right = right.sext(Type.int(32))
            return getattr(left, method)(right)


class SingleExprNode(ASTNode):
    def __init__(self, context, operator, right):
        ASTNode.__init__(self, context)
        self.operator = operator
        self.right = right

    def code_gen(self):
        value = self.right.code_gen()
        if self.operator.word == '-':
            if value.type == Type.int(32) or value.type == Type.int(8):
                return Constant.int(value.type, '0').sub(value)
            elif value.type == Type.double():
                return Constant.real(value.type, '0').fsub(value)
        else:
            if value.type == Type.int(32) or value.type == Type.int(8):
                return Constant.int(value.type, '0').icmp(value)
            elif value.type == Type.double():
                return Constant.real(value.type, '0').fcmp(value)


class FuncPrototypeNode(ASTNode):
    def __init__(self, context, func_name_token, ret_type, args):
        ASTNode.__init__(self, context)
        self.func_name_token = func_name_token
        self.ret_type = ret_type
        self.args = args

    def code_gen(self, from_definition=False):
        top_context = self.context.parent_context
        func_name_with_tag = self.func_name_token.word + "()"
        return_type = Helper.get_type(self.ret_type.word)
        arg_types = [Helper.get_type(arg[1]) for arg in self.args if True]
        func_type = Type.function(return_type, arg_types, False)
        if not func_name_with_tag in top_context.type_table:

            function = Function.new(g_llvm_module, func_type, self.func_name_token.word)
            top_context.type_table[func_name_with_tag] = func_type
            for arg in self.args:
                self.context.type_table[arg[0]] = Helper.get_type(arg[1])
            return function
        else:
            old_func_type = top_context.type_table[func_name_with_tag]
            if old_func_type == func_type:
                if from_definition:
                    return g_llvm_module.get_function_named(self.func_name_token.word)
                else:
                    raise cmexception.RedefineException(self.func_name_token, 'function')
            else:
                raise cmexception.RedefineException(self.func_name_token, 'function')


class FunctionNode(ASTNode):
    def __init__(self, context, prototype, body):
        ASTNode.__init__(self, context)
        self.prototype = prototype
        self.body = body

    def code_gen(self):
        function_prototype = self.prototype.code_gen(True)
        block = function_prototype.append_basic_block('entry')
        global g_llvm_builder
        g_llvm_builder = Builder.new(block)
        if self.body:
            for stmt in self.body:
                stmt.code_gen()


class IfElseNode(ASTNode):
    def __init__(self, context, condition, if_node, else_node):
        ASTNode.__init__(self, context)
        self.condition = condition
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
    def __init__(self, context, constant_token):
        ASTNode.__init__(self, context)
        self.constant_token = constant_token

    def code_gen(self):
        if self.constant_token.lexme_type == LexmeType.Integer:
            return Constant.int(Helper.get_type(self.constant_token.lexme_type), self.constant_token.word)
        elif self.constant_token.lexme_type == LexmeType.Double:
            return Constant.real(Helper.get_type(self.constant_token.lexme_type), self.constant_token.word)
        elif self.constant_token.lexme_type == LexmeType.String:
            s = self.constant_token.word.strip('"')
            global constant_string_num
            global_string = GlobalVariable.new(g_llvm_module, Type.array(Type.int(8), len(s) + 1),
                                               ".str%d" % constant_string_num)
            constant_string_num += 1
            global_string.initializer = Constant.stringz(s)
            return global_string
        elif self.constant_token.lexme_type == LexmeType.Char:
            ascii = ord(self.constant_token.word.strip("'"))
            return Constant.int(Helper.get_type(self.constant_token.lexme_type), ascii)


class VariableNode(ASTNode):
    def __init__(self, context, var_name_token):
        ASTNode.__init__(self, context)
        self.var_name_token = var_name_token

    def code_gen(self):
        pass


class ArrayVariableNode(ASTNode):
    def __init__(self, context, array_name_token, index_expr_node):
        ASTNode.__init__(self, context)
        self.array_name_token = array_name_token
        self.index_expr_node = index_expr_node

    def code_gen(self):
        pass


class FunctionCallNode(ASTNode):
    def __init__(self, context, func_name_token, args):
        ASTNode.__init__(self, context)
        self.func_name_token = func_name_token
        self.args = args

    def code_gen(self):
        pass


class Helper:
    def __init__(self):
        pass

    @staticmethod
    def get_type(typo):
        if typo == 'int' or typo == LexmeType.Integer:
            return Type.int(32)
        elif typo == 'double' or typo == LexmeType.Double:
            return Type.double()
        elif typo == 'String' or typo == LexmeType.String:
            ch = Type.int(8)
            return Type.pointer(ch)
        elif typo == 'char' or typo == LexmeType.Char:
            return Type.int(8)
        elif typo == 'void':
            return Type.void()

    @staticmethod
    def get_array_type(typo, length):
        if typo == 'int':
            return Type.array(Type.int(32), length)
        elif typo == 'double':
            return Type.array(Type.double(), length)
        elif typo == 'String':
            ch = Type.int(8)
            return Type.array(Type.pointer(ch), length)
        elif typo == 'char':
            return Type.array(Type.int(8), length)
