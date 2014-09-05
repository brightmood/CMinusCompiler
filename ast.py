from llvm.core import Module, Constant, Type, Function, Builder, GlobalVariable, ConstantArray
from llvm.core import IPRED_EQ, IPRED_NE, IPRED_SGT, IPRED_SGE, IPRED_SLT, IPRED_SLE
from llvm.core import RPRED_UEQ, RPRED_UGT, RPRED_UGE, RPRED_ULT, RPRED_ULE, RPRED_UNE
import cmexception
from lexer import LexmeType

g_llvm_module = Module.new('CMCompiler')
g_llvm_builder = None
func_table = {}
constant_string_num = 0


#Base class of each Abstract Syntax Tree Node
class ASTNode:
    def __init__(self, context):
        self.context = context

    def print_ast(self):
        pass

    #each node can implements this method to generate LLVMIR code
    def code_gen(self):
        pass


#Root node, contains a list of functions and global statements
class ProgramNode(ASTNode):
    def __init__(self, context, nodes):
        ASTNode.__init__(self, context)
        self.nodes = nodes

    def code_gen(self):
        for node in self.nodes:
            node.code_gen()
        return g_llvm_module


#Declare a variable
class VarDeclNode(ASTNode):

    def __init__(self, context, var_name_token, typo):
        ASTNode.__init__(self, context)
        self.var_name_token = var_name_token
        self.typo = typo

    #If parent context is none, this declare statement is a global statement.
    #Register it as a global variable using GlobalVariable.new.
    #Or register it as a local variable
    def code_gen(self):

        if self.context.parent_context is None:
            if self.var_name_token.word in self.context.type_table:
                raise cmexception.RedefineException(self.var_name_token, 'Global Variable')
            else:
                t = Helper.get_type(self.typo.word)
                gv = GlobalVariable.new(g_llvm_module, t, self.var_name_token.word)
                self.context.type_table[self.var_name_token.word] = t
                self.context.value_table[self.var_name_token.word] = gv
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


#Declare an array
class ArrayDeclNode(ASTNode):
    def __init__(self, context, array_name_token, typo, length_token):
        ASTNode.__init__(self, context)
        self.array_name_token = array_name_token
        self.typo = typo
        self.length_token = length_token

    #If parent context is none, this declare statement is a global statement.
    #Register it as a global array using GlobalVariable.new.
    #Or register it as a local array
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
                self.context.value_table[self.array_name_token.word] = gv
        else:
            if not self.array_name_token.word in self.context.type_table:
                t = Helper.get_array_type(self.typo.word, length)
                var_addr = g_llvm_builder.alloca(t)
                self.context.type_table[self.array_name_token.word] = t
                self.context.value_table[self.array_name_token.word] = var_addr
            else:
                raise cmexception.RedefineException(self.array_name_token)


#Assign a variable
class VarAssignNode(ASTNode):
    def __init__(self, context, var_name_token, expr_node):
        ASTNode.__init__(self, context)
        self.var_name_token = var_name_token
        self.expr_node = expr_node

    #Two points
    #1. If the assigned value is a constant string such as "hello world",
    #we should use gep to get the primary address of an array, since a string
    #in llvm is an array of characters.
    #2. Assignment implicits auto up-cast, we use Help.auto_cast to achieve it.
    def code_gen(self):
        if self.var_name_token.word in self.context.type_table:
            var_type = self.context.type_table[self.var_name_token.word]
            value_addr = self.context.value_table[self.var_name_token.word]
            expr = self.expr_node.code_gen()
            if expr in g_llvm_module.global_variables:
                expr = g_llvm_builder.gep(expr, [Constant.int(Type.int(32), 0), Constant.int(Type.int(32), 0)])
            casted_expr = Helper.auto_cast(g_llvm_builder, expr, var_type)
            if casted_expr:
                g_llvm_builder.store(casted_expr, value_addr)
            else:
                raise cmexception.CastException(self.var_name_token, var_type, expr.type)
        else:
            if self.var_name_token.word in self.context.parent_context.type_table:
                gv = g_llvm_module.get_global_variable_named(self.var_name_token.word)
                expr = self.expr_node.code_gen()
                g_llvm_builder.store(expr, gv)
            else:
                raise cmexception.NotDefinedException(self.var_name_token, 'Variable')


#Assign a array element
class ArrayAssignNode(ASTNode):
    def __init__(self, context, array_name_token, index_expr_node, value_expr_node):
        ASTNode.__init__(self, context)
        self.array_name_token = array_name_token
        self.index_expr_node = index_expr_node
        self.value_expr_node = value_expr_node

    #Basically as same as variable assignment. It should be noticed that
    #it checks the array out of bound error.
    def code_gen(self):
        index = self.index_expr_node.code_gen()
        expr = self.value_expr_node.code_gen()
        if self.array_name_token.word in self.context.type_table:
            t = self.context.type_table[self.array_name_token.word]
            if t.count <= index.z_ext_value:
                raise cmexception.ArrayIndexOutOfBoundException(self.index_expr_node.constant_token, t.count)
            value_address = self.context.value_table[self.array_name_token.word]
            address = g_llvm_builder.gep(value_address, [Constant.int(Type.int(32), 0), index])
            g_llvm_builder.store(expr, address)
        else:
            if self.array_name_token.word in self.context.parent_context.type_table:
                t = self.context.parent_context.type_table[self.array_name_token.word]
                if t.count <= index.z_ext_value:
                    raise cmexception.ArrayIndexOutOfBoundException(self.index_expr_node.constant_token, t.count)
                global_array = g_llvm_module.get_global_variable_named(self.array_name_token.word)
                address = g_llvm_builder.gep(global_array, [Constant.int(Type.int(32), 0), index])
                g_llvm_builder.store(expr, address)
            else:
                raise cmexception.NotDefinedException(self.array_name_token, 'Array')


#Declare and initial a variable
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
                    self.context.value_table[self.var_name_token.word] = expr
                else:
                    gv = GlobalVariable.new(g_llvm_module, t, self.var_name_token.word)
                    gv.initializer = expr
                    self.context.value_table[self.var_name_token.word] = gv
                self.context.type_table[self.var_name_token.word] = t

        else:
            if not self.var_name_token.word in self.context.type_table:
                t = Helper.get_type(self.typo.word)
                var_address = g_llvm_builder.alloca(t)
                self.context.type_table[self.var_name_token.word] = t
                self.context.value_table[self.var_name_token.word] = var_address
                expr = self.expr_node.code_gen()
                if expr in g_llvm_module.global_variables:
                    expr = g_llvm_builder.gep(expr, [Constant.int(Type.int(32), 0), Constant.int(Type.int(32), 0)])
                casted_expr = Helper.auto_cast(g_llvm_builder, expr, t)
                if casted_expr:
                    g_llvm_builder.store(casted_expr, var_address)
                else:
                    raise cmexception.CastException(self.var_name_token, t, expr.type)
            else:
                raise cmexception.RedefineException(self.var_name_token)


#Declare and initial an array
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
                self.context.value_table[self.array_name_token.word] = gv
        else:
            if self.array_name_token.word in self.context.type_table:
                raise cmexception.RedefineException(self.array_name_token)
            else:
                t = Helper.get_array_type(self.typo.word, int(self.length.word))
                array_address = g_llvm_builder.alloca(t)
                inx = 0
                for i in self.initial_value:
                    value = i.code_gen()
                    if value in g_llvm_module.global_variables:
                        string_value_ptr = g_llvm_builder.gep(
                            value, [Constant.int(Type.int(32), 0), Constant.int(Type.int(32), 0)])
                        var_address = g_llvm_builder.gep(
                            array_address, [Constant.int(Type.int(32), 0), Constant.int(Type.int(32), inx)])
                        g_llvm_builder.store(string_value_ptr, var_address)
                    else:
                        var_address = g_llvm_builder.gep(
                            array_address, [Constant.int(Type.int(32), 0), Constant.int(Type.int(32), inx)])
                        g_llvm_builder.store(value, var_address)
                    inx += 1
                self.context.type_table[self.array_name_token.word] = t
                self.context.value_table[self.array_name_token.word] = array_address


class BinaryExprNode(ASTNode):
    def __init__(self, context, left, operator, right):
        ASTNode.__init__(self, context)
        self.left = left
        self.operator = operator
        self.right = right

    def code_gen(self):

        d = Type.double()
        i32 = Type.int(32)
        i8 = Type.int(8)
        i1 = Type.int(1)

        op = self.operator.word
        left = self.left.code_gen()
        right = self.right.code_gen()

        if op == '||' or op == '&&':
            if left.type == d:
                bool_left = Constant.real(left.type, '0').fcmp(RPRED_UEQ, left)
            else:
                bool_left = Constant.int(left.type, '0').fcmp(IPRED_EQ, left)
            if right.type == d:
                bool_right = Constant.real(left.type, '0').fcmp(RPRED_UEQ, right)
            else:
                bool_right = Constant.int(left.type, '0').fcmp(IPRED_EQ, right)
            if op == '||':
                if bool_left == bool_right == Constant.int(i1, 0):
                    return bool_right
                else:
                    return Constant.int(i1, 1)
            elif op == '&&':
                if bool_left == bool_right == Constant.int(i1, 1):
                    return bool_right
                else:
                    return Constant.int(i1, 0)
        method = Helper.choose_method(left, op, right)
        # if g_llvm_builder:
        if method[0] == 'f':
            if left.type != d:
                left = left.sitofp(d)
            if right.type != d:
                right = right.sitofp(d)
        elif method == 'and_' or method == 'or_':
            if left.type == d or right.type == d:
                raise cmexception.InvalidOperandException(self.operator, str(left.type), str(right.type))
            else:
                if left.type != right.type:
                    if left.type == i1:
                        left = left.zext(Type.int(32))
                    elif left.type != i32:
                        left = left.sext(Type.int(32))
                    if right.type == i1:
                        right = right.zext(Type.int(32))
                    elif right.type != i32:
                        right = right.sext(Type.int(32))
        else:
            if left.type != right.type:
                if left.type == i1:
                    left = left.zext(Type.int(32))
                elif left.type != i32:
                    left = left.sext(Type.int(32))
                if right.type == i1:
                    right = right.zext(Type.int(32))
                elif right.type != i32:
                    right = right.sext(Type.int(32))
        if op == '<' or op == '>' or op == '<=' or op == '>=' or op == '==' or op == '!=':
            flag = Helper.choose_flag(op, left)
            if g_llvm_builder is None:
                return getattr(left, method)(flag, right)
            else:
                return getattr(g_llvm_builder, method)(flag, left, right)
        else:
            if g_llvm_builder is None:
                return getattr(left, method)(right)
            else:
                return getattr(g_llvm_builder, method)(left, right)


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
            if value.type == Type.int(32) or value.type == Type.int(8) or Type.int(1):
                return Constant.int(value.type, '0').icmp(IPRED_EQ, value)
            elif value.type == Type.double():
                return Constant.real(value.type, '0').fcmp(RPRED_UEQ, value)


class CastExprNode(ASTNode):

    def __init__(self, context, expr_node, target_type_token):
        ASTNode.__init__(self, context)
        self.context = context
        self.expr_node = expr_node
        self.target_type_token = target_type_token

    def code_gen(self):
        value = self.expr_node.code_gen()
        value = Helper.force_cast(g_llvm_builder, value, Helper.get_type(self.target_type_token.word))
        if value:
            return value
        else:
            raise cmexception.CastException(self.target_type_token, self.target_type_token.word, value.type)


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
            return [function, self.context]
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

        function_prototype_and_context = self.prototype.code_gen(True)
        function_prototype = function_prototype_and_context[0]
        context = function_prototype_and_context[1]
        block = function_prototype.append_basic_block('entry')
        global g_llvm_builder
        g_llvm_builder = Builder.new(block)
        for arg in self.prototype.args:
            t = context.type_table[arg[0]]
            context.value_table[arg[0]] = g_llvm_builder.alloca(t)
        return_value = None
        return_token = None
        if self.body:
            for stmt in self.body:
                return_value = stmt.code_gen()
                if return_value is not None:
                    return_token = stmt.return_token
                    break
        key = self.prototype.func_name_token.word + '()'
        expected_return_type = self.context.type_table[key].return_type

        if return_value is not None and return_value == Type.void():
            if expected_return_type == Type.void():
                g_llvm_builder.ret_void()
            else:
                raise cmexception.FunctionReturnTypeNotMatchedException(
                    self.prototype.func_name_token, return_token,
                    Helper.get_type_string(expected_return_type), return_value)
        elif return_value is not None:
            if expected_return_type == return_value.type:
                g_llvm_builder.ret(return_value)
            else:
                raise cmexception.FunctionReturnTypeNotMatchedException(
                    self.prototype.func_name_token, return_token,
                    Helper.get_type_string(expected_return_type), return_value.type)
        else:
            if expected_return_type == Type.void():
                g_llvm_builder.ret_void()
            else:
                raise cmexception.FunctionReturnTypeNotMatchedException(
                    self.prototype.func_name_token, None,
                    Helper.get_type_string(expected_return_type), Type.void())

        # Validate the generated code, checking for consistency.
        try:
            function_prototype.verify()
        except:
            function_prototype.delete()


class IfElseNode(ASTNode):
    def __init__(self, context, condition, if_node, else_node):
        ASTNode.__init__(self, context)
        self.condition = condition
        self.if_node = if_node
        self.else_node = else_node

    def code_gen(self):
        condition = self.condition.code_gen()

        if str(condition.type) == 'double':
            value = condition.fcmp(RPRED_UNE, Constant.real(condition.type, 0))
        else:
            value = condition.icmp(IPRED_NE, Constant.int(condition.type, 0))
        if value.z_ext_value == 1:
            self.if_node.code_gen()
        else:
            self.else_node.code_gen()


class IfNode(ASTNode):
    def __init__(self, context, stmts):
        ASTNode.__init__(self, context)
        self.stmts = stmts

    def code_gen(self):
        if self.stmts:
            for stmt in self.stmts:
                stmt.code_gen()


class ElseNode(ASTNode):
    def __init__(self, context, stmts):
        ASTNode.__init__(self, context)
        self.stmts = stmts

    def code_gen(self):
        if self.stmts:
            for stmt in self.stmts:
                stmt.code_gen()


class WhileNode(ASTNode):
    def __init__(self, context, expr, statements_node):
        ASTNode.__init__(self, context)
        self.expr = expr
        self.statements_node = statements_node

    def code_gen(self):
        pass


class ReturnNode(ASTNode):
    def __init__(self, context, return_token, expr_or_string_node):
        ASTNode.__init__(self, context)
        self.return_token = return_token
        self.expr_or_string_node = expr_or_string_node

    def code_gen(self):
        if self.expr_or_string_node is not None:
            return self.expr_or_string_node.code_gen()
        else:
            return Type.void()


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
        if self.var_name_token.word in self.context.type_table:
            t = self.context.type_table[self.var_name_token.word]
            if str(t) == 'i8*':
                return self.context.value_table[self.var_name_token.word]
            else:
                return g_llvm_builder.load(
                    self.context.value_table[self.var_name_token.word])
        else:
            if self.context.parent_context is not None:
                if self.var_name_token.word in self.context.parent_context.type_table:
                    t = self.context.parent_context.type_table[self.var_name_token.word]
                    if str(t) == 'i8*':
                        return self.context.parent_context.value_table[self.var_name_token.word]
                    else:
                        return g_llvm_builder.load(
                            self.context.parent_context.value_table[self.var_name_token.word])
                else:
                    raise cmexception.NotDefinedException(self.var_name_token)
            else:
                raise cmexception.NotDefinedException(self.var_name_token)


class ArrayVariableNode(ASTNode):
    def __init__(self, context, array_name_token, index_expr_node):
        ASTNode.__init__(self, context)
        self.array_name_token = array_name_token
        self.index_expr_node = index_expr_node

    def code_gen(self):
        if self.array_name_token.word in self.context.type_table:
            t = self.context.type_table[self.array_name_token.word]
            array_address = self.context.value_table[self.array_name_token.word]
            element_address = g_llvm_builder.gep(array_address,
                               [Constant.int(Type.int(32), 0), self.index_expr_node.code_gen()])
            return g_llvm_builder.load(element_address)
        else:
            if self.context.parent_context is not None:
                if self.array_name_token.word in self.context.parent_context.type_table:
                    t = self.context.parent_context.type_table[self.array_name_token.word]
                    if str(t) == 'i8*':
                        return self.context.parent_context.value_table[self.array_name_token.word]
                    else:
                        return g_llvm_builder.load(
                            self.context.parent_context.value_table[self.array_name_token.word])
                else:
                    raise cmexception.NotDefinedException(self.array_name_token)
            else:
                raise cmexception.NotDefinedException(self.array_name_token)


class FunctionCallNode(ASTNode):
    def __init__(self, context, func_name_token, args):
        ASTNode.__init__(self, context)
        self.func_name_token = func_name_token
        self.args = args

    def code_gen(self):
        callee = g_llvm_module.get_function_named(self.func_name_token.word)
        if len(callee.args) != len(self.args):
            raise RuntimeError('Incorrect number of arguments passed.')
        return g_llvm_builder.call(callee, [arg.code_gen() for arg in self.args], 'calltmp')


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

    @staticmethod
    def choose_method(left, op, right):
        method_table = {
            'i': {'+': 'add', '-': 'sub', '*': 'mul', '/': 'sdiv',
                  '%': 'srem', '<<': 'shl', '>>': 'lshr',
                  '<': 'icmp', '>': 'icmp', '<=': 'icmp', '>=': 'icmp',
                  '==': 'icmp', '!=': 'icmp'
            },
            'f': {'+': 'fadd', '-': 'fsub', '*': 'fmul', '/': 'fdiv',
                  '%': 'frem', '<': 'fcmp', '>': 'fcmp', '<=': 'fcmp',
                  '>=': 'fcmp', '==': 'fcmp', '!=': 'fcmp'
            }
        }
        if op == '|':
            return 'or_'
        if op == '&':
            return 'and_'
        if left.type == Type.double() or right.type == Type.double():
            return method_table['f'][op]
        return method_table['i'][op]

    @staticmethod
    def force_cast(builder, value, target_type):
        if value.type != target_type:
            d = Type.double()
            i32 = Type.int(32)
            i8 = Type.int(8)
            i1 = Type.int(1)
            if target_type == d:
                if value.type != d:
                    # if builder:
                    #     value = builder.sitofp(value, d)
                    # else:
                    value = value.sitofp(d)
            elif target_type == i32:
                if value.type == d:
                    # if builder:
                    #     value = builder.fptosi(value, i32)
                    # else:
                    value = value.fptosi(i32)
                elif value.type == i1:
                    # if builder:
                    #     value = builder.zext(value, i32)
                    # else:
                    value = value.zext(i32)
                else:
                    # if builder:
                    #     value = builder.sext(value, i32)
                    # else:
                    value = value.sext(i32)
            elif target_type == i8:
                if value.type == d:
                    # if builder:
                    #     value = builder.fptrunc(value, i8)
                    # else:
                    value = value.fptrunc(i8)
                elif value.type == i32:
                    # if builder:
                    #     value = builder.trunc(value, i8)
                    # else:
                    value = value.trunc(i8)
                elif value.type == i1:
                    # if builder:
                    #     value = builder.zext(value, i8)
                    # else:
                    value = value.zext(i8)
            elif target_type == Type.pointer(Type.int(8)):
                return None
        return value

    @staticmethod
    def auto_cast(builder, value, target_type):
        if value.type != target_type:
            d = Type.double()
            i32 = Type.int(32)
            i8 = Type.int(8)
            i1 = Type.int(1)
            if target_type == d:
                if value.type != d:
                    # if builder:
                    #     value = builder.sitofp(value, d)
                    # else:
                    value = value.sitofp(d)
            elif target_type == i32:
                if value.type == d:
                    return None
                elif value.type == i1:
                    # if builder:
                    #     value = builder.zext(value, i32)
                    # else:
                        value = value.zext(i32)
                else:
                    # if builder:
                    #     value = builder.sext(value, i32)
                    # else:
                    value = value.sext(i32)
            elif target_type == i8:
                if value.type == i1:
                    # if builder:
                    #     value = builder.zext(value, i8)
                    # else:
                    value = value.zext(i8)
                else:
                    return None
            elif target_type == Type.pointer(Type.int(8)):
                return None
        return value

    @staticmethod
    def choose_flag(op, left):
        table = {
            'i': {'<': IPRED_SLT, '>': IPRED_SGT, '<=': IPRED_SLE, '>=': IPRED_SGE,
                  '==': IPRED_EQ, '!=': IPRED_NE
            },
            'f': {'<': RPRED_ULT, '>': RPRED_UGT, '<=': RPRED_ULE, '>=': RPRED_UGE,
                  '==': RPRED_UEQ, '!=': RPRED_UNE
            }
        }
        if str(left.type) == 'double':
            return table['f'][op]
        else:
            return table['i'][op]

    @staticmethod
    def get_type_string(ty):
        i8 = Type.int(8)
        i32 = Type.int(32)
        d = Type.double()
        string = Type.pointer(i8)
        if ty == i8:
            return 'char'
        elif ty == i32:
            return 'int'
        elif ty == d:
            return 'double'
        elif ty == string:
            return 'String'