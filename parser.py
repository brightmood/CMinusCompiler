# coding=utf-8

from lexer import LexmeType
import ast
import cmexception


class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        self.llk = []
        self.llk.append(self.lexer.get_next_token())
        self.context = ASTContext(None)
        # self.current_node = None
        # self.root_node = None

    def lookup_for_n(self, n):
        length = len(self.llk)
        if n > length - 1:
            for x in range(0, n + 1 - length):
                self.llk.append(self.lexer.get_next_token())
        return self.llk[n]

    def move_for_n(self, n):
        self.lookup_for_n(n)
        self.llk = self.llk[n:]
        return self.llk[0]

    def get_current_token(self):
        return self.llk[0]

    def process(self):
        return self.parse_program()

    def parse_program(self):
        # self.current_node = ast.ProgramNode(self.context)
        # self.root_node = self.current_node
        node = self.parse_global()
        nodes = self.parse_e()
        if nodes is not None:
            nodes.insert(0, node)
        else:
            nodes = [node]
        program_node = ast.ProgramNode(self.context, nodes)
        return program_node

    def parse_e(self):
        token = self.get_current_token()
        nodes = []
        if token is None:
            return
        if token.word == '':
            return
        else:
            nodes.append(self.parse_global())
            rest = self.parse_e()
            if rest is not None:
                nodes.extend(rest)
            return nodes

    def parse_global(self):
        look_forward2 = self.lookup_for_n(2)
        if look_forward2.word == '(':
            prototype_node = self.parse_func_prototype()
            current = self.get_current_token()
            if current.word == ';':
                self.context = self.context.parent_context
                self.move_for_n(1)
                return prototype_node
            elif current.word == '{':
                body = self.parse_statement()
                self.context = self.context.parent_context
                return ast.FunctionNode(self.context, prototype_node, body)
            else:
                raise cmexception.SyntaxException(current, ['{', ';'])
        else:
            return self.parse_global_statement()

    def parse_global_statement(self):
        token = self.get_current_token()
        if token.is_type():
            look_forward_2 = self.lookup_for_n(2)
            if look_forward_2.word == ';':
                return self.parse_var_decl()
            elif look_forward_2.word == '[':
                look_forward_5 = self.lookup_for_n(5)
                if look_forward_5.word == ';':
                    return self.parse_array_decl()
                else:
                    return self.parse_array_decl_and_assign()
            elif look_forward_2.word == '=':
                return self.parse_var_decl_and_assign()
            else:
                raise cmexception.SyntaxException(look_forward_2, [';', '[', '='])
        else:
            raise cmexception.SyntaxException(token, ['int', 'double', 'String', 'char'])

    def parse_statement(self):
        token = self.get_current_token()
        if token.is_type():
            lookup2 = self.lookup_for_n(2)
            if lookup2.word == ';':
                return self.parse_var_decl()
            elif lookup2.word == '=':
                return self.parse_var_decl_and_assign()
            elif lookup2.word == '[':
                lookup5 = self.lookup_for_n(5)
                if lookup5.word == '=':
                    return self.parse_array_decl_and_assign()
                elif lookup5.word == ';':
                    return self.parse_array_decl()
                else:
                    raise cmexception.SyntaxException(lookup5, ['=', ';'])
            else:
                raise cmexception.SyntaxException(lookup2, [';', '=', '['])
        elif token.lexme_type == LexmeType.Variable:
            lookup1 = self.lookup_for_n(1)
            if lookup1.word == '[':
                return self.parse_array_assign()
            elif lookup1.word == '=':
                return self.parse_var_assign()
            elif lookup1.word == '(':
                return self.parse_function_call()
            else:
                raise cmexception.SyntaxException(lookup1, ['=', '['])
        elif token.word == '{':
            self.move_for_n(1)
            nodes = self.parse_stmtlist()
            right_bracket = self.get_current_token()
            if right_bracket.word == '}':
                self.move_for_n(1)
                return nodes
            else:
                raise cmexception.SyntaxException(right_bracket, ['}'])
        elif token.word == 'if':
            left_brace = self.move_for_n(1)
            if left_brace.word == '(':
                self.move_for_n(1)
                condition_expr = self.parse_expr()
                right_brace = self.get_current_token()
                if right_brace.word == ')':
                    self.move_for_n(1)
                    if_node = self.parse_statement()
                    else_node = self.parse_else_statement()
                    return ast.IfElseNode(self.context, condition_expr, if_node, else_node)
                else:
                    raise cmexception.SyntaxException(right_brace, [')'])
            else:
                raise cmexception.SyntaxException(left_brace, ['('])
        elif token.word == 'while':
            left_brace = self.lookup_for_n(1)
            if left_brace.word == '(':
                self.move_for_n(2)
                expr_node = self.parse_expr()
                right_brace = self.get_current_token()
                if right_brace.word == ')':
                    self.move_for_n(1)
                    stmtnode = self.parse_statement()
                    return ast.WhileNode(self.context, expr_node, stmtnode)
                else:
                    raise cmexception.SyntaxException(right_brace, [')'])
            else:
                raise cmexception.SyntaxException(left_brace, ['('])
        elif token.word == 'return':
            self.move_for_n(1)
            node = self.parse_expr_or_string()
            if self.get_current_token().word == ';':
                self.move_for_n(1)
                return ast.ReturnNode(self.context, node)
        else:
            raise cmexception.SyntaxException(token, ['<legal identifier>', 'while', 'return', 'if', '<type>'])

    def parse_stmtlist(self):
        nodes = []
        token = self.get_current_token()
        if token.word == '}':
            return
        else:
            nodes.append(self.parse_statement())
            stmtnodes = self.parse_stmtlist()
            if stmtnodes is not None:
                nodes.extend(stmtnodes)
            return nodes

    def parse_else_statement(self):
        token = self.get_current_token()
        if token.word == 'else':
            self.move_for_n(1)
            return ast.ElseNode(self.context, self.parse_statement())
        else:
            return

    def parse_function_call(self):
        token = self.get_current_token()
        left_brace = self.lookup_for_n(1)
        if left_brace.word == '(':
            self.move_for_n(2)
            valuelist = self.parse_array_value_list()
            right_brace = self.get_current_token()
            if right_brace.word == ')':
                semicolon = self.move_for_n(1)
                if semicolon.word == ';':
                    return ast.FunctionCallNode(self.context, token, valuelist)
                else:
                    raise cmexception.SyntaxException(semicolon, [';'])
            else:
                raise cmexception.SyntaxException(right_brace, [')'])
        else:
            raise cmexception.SyntaxException(left_brace, ['('])

    # def parse_function(self):
    #     func_node = ast.FunctionNode(self.context, self.parse_func_prototype(), self.parse_statement())
    #     self.context = self.context.parent_context
    #     return func_node

    def parse_func_prototype(self):
        ret_type_token = self.get_current_token()
        if ret_type_token.is_type() or ret_type_token.word == 'void':
            func_name_token = self.lookup_for_n(1)
            if func_name_token.lexme_type == LexmeType.Variable:
                self.move_for_n(3)
                args = self.parse_paramlist()
                right_bracket = self.get_current_token()
                if right_bracket.word == ')':
                    self.move_for_n(1)
                    temp = self.context
                    self.context = ASTContext(temp)
                    # for arg in args:
                    #     self.context.type_table[arg[0]] = arg[1]
                    return ast.FuncPrototypeNode(self.context, func_name_token, ret_type_token, args)
                else:
                    raise cmexception.SyntaxException(right_bracket, ['<legal identifier>'])
            else:
                raise cmexception.SyntaxException(func_name_token, ['<legal identifier>'])
        else:
            raise cmexception.SyntaxException(ret_type_token, ['int', 'double', 'String', 'void', 'char'])

    def parse_paramlist(self):
        token = self.get_current_token()
        nodes = []
        if token.is_type():
            var_token = self.lookup_for_n(1)
            if var_token.lexme_type == LexmeType.Variable:
                nodes.append((var_token.word, token.word))
                self.move_for_n(2)
                nodes.extend(self.parse_paramlistrest())
                return nodes
            else:
                raise cmexception.SyntaxException(var_token, ['<legal identifier>'])
        elif token.word == ')':
            return nodes

    def parse_paramlistrest(self):
        nodes = []
        token = self.get_current_token()
        if token.word == ',':
            type_token = self.lookup_for_n(1)
            if type_token.is_type():
                var_token = self.lookup_for_n(2)
                if var_token.lexme_type == LexmeType.Variable:
                    self.move_for_n(3)
                    nodes.append((var_token.word, type_token.word))
                    nodes.extend(self.parse_paramlistrest())
                    return nodes
                else:
                    raise cmexception.SyntaxException(var_token, ['<legal identifier>'])
            else:
                raise cmexception.SyntaxException(type_token, ['<argument type>'])
        elif token.word == ')':
            return nodes

    def parse_var_decl(self):
        type_token = self.get_current_token()
        var_token = self.lookup_for_n(1)
        if var_token.lexme_type == LexmeType.Variable:
            node = ast.VarDeclNode(self.context, var_token, type_token)
            self.move_for_n(3)
            return node
        else:
            raise cmexception.SyntaxException(var_token, ['<legal identifier>'])

    def parse_array_decl(self):
        type_token = self.get_current_token()
        var_token = self.lookup_for_n(1)
        length_token = self.lookup_for_n(3)
        if var_token.lexme_type == LexmeType.Variable:
            if length_token.lexme_type == LexmeType.Integer:
                if self.lookup_for_n(4).word == ']':
                    if self.lookup_for_n(5).word == ';':
                        node = ast.ArrayDeclNode(self.context, var_token, type_token, length_token)
                        self.move_for_n(6)
                        return node
                    else:
                        raise cmexception.SyntaxException(self.lookup_for_n(5), [';'])
                else:
                    raise cmexception.SyntaxException(self.lookup_for_n(4), [']'])
            else:
                raise cmexception.SyntaxException(length_token, ['<an positive Integer>'])
        else:
            raise cmexception.SyntaxException(var_token, ['<legal identifier>'])

    def parse_var_assign(self):
        var_token = self.get_current_token()
        assign = self.lookup_for_n(1)
        if assign.word == '=':
            self.move_for_n(2)

            # self.current_node.children.append(node)
            # self.current_node = node
            expr_or_string_node = self.parse_expr_or_string()
            semicolon = self.get_current_token()
            if semicolon.word == ';':
                node = ast.VarAssignNode(self.context, var_token, expr_or_string_node)
                self.move_for_n(1)
                return node
            else:
                raise cmexception.SyntaxException(semicolon, [';'])
        else:
            raise cmexception.SyntaxException(assign, ['='])

    def parse_array_assign(self):
        var_token = self.get_current_token()

        # self.current_node.children.append(node)
        # self.current_node = node
        left_bracket = self.lookup_for_n(1)
        if left_bracket.word == '[':
            self.move_for_n(2)
            index_expr_node = self.parse_expr()
            right_bracket = self.get_current_token()
            if right_bracket.word == ']':
                equal = self.lookup_for_n(1)
                if equal.word == '=':
                    self.move_for_n(2)
                    value_expr_node = self.parse_expr_or_string()
                    node = ast.ArrayAssignNode(self.context, var_token, index_expr_node, value_expr_node)
                    semicolon = self.get_current_token()
                    if semicolon.word == ';':
                        self.move_for_n(1)
                        return node
                    else:
                        raise cmexception.SyntaxException(semicolon, [';'])
                else:
                    raise cmexception.SyntaxException(equal, ['='])
            else:
                raise cmexception.SyntaxException(right_bracket, [']'])
        else:
            raise cmexception.SyntaxException(left_bracket, ['['])

    def parse_var_decl_and_assign(self):
        type_token = self.get_current_token()
        var_token = self.lookup_for_n(1)
        if var_token.lexme_type == LexmeType.Variable:
            self.move_for_n(3)
            expr_node = self.parse_expr_or_string()
            semicolon = self.get_current_token()
            if semicolon.word == ';':
                self.move_for_n(1)
                return ast.VarDeclAndAssignNode(self.context, var_token, type_token, expr_node)
            else:
                raise cmexception.SyntaxException(semicolon, [';'])
        else:
            raise cmexception.SyntaxException(self.lookup_for_n(4), ['<legal identifier>'])

    def parse_array_decl_and_assign(self):
        type_token = self.get_current_token()
        var_token = self.lookup_for_n(1)
        if var_token.lexme_type == LexmeType.Variable:
            length_token = self.lookup_for_n(3)
            if length_token.lexme_type == LexmeType.Integer:
                self.move_for_n(6)
                initial_value = self.parse_array_initial_value()
                semicolon = self.get_current_token()
                if semicolon.word == ';':
                    self.move_for_n(1)
                    return ast.ArrayDeclAndAssignNode(self.context, var_token, type_token, length_token,
                                                      initial_value)
                else:
                    raise cmexception.SyntaxException(self.lookup_for_n(4), [';'])
            else:
                raise cmexception.SyntaxException(self.lookup_for_n(4), ['<positive integer>'])
        else:
            raise cmexception.SyntaxException(self.lookup_for_n(4), ['<legal identifier>'])

    def parse_array_initial_value(self):
        token = self.get_current_token()
        if token.word == '{':
            self.move_for_n(1)
            valuelist = self.parse_array_value_list()
            token = self.get_current_token()
            if token.word == '}':
                self.move_for_n(1)
                return valuelist
            else:
                raise cmexception.SyntaxException(self.lookup_for_n(4), ['}'])
        else:
            raise cmexception.SyntaxException(self.lookup_for_n(4), ['{'])

    def parse_array_value_list(self):
        valuelist = []
        token = self.get_current_token()
        if token.word == '}':
            return
        else:
            valuelist.append(self.parse_expr_or_string())
            valuelist.extend(self.parse_comma_expr_list())
            return valuelist

    def parse_comma_expr_list(self):
        valuelist = []
        token = self.get_current_token()
        if token.word == ',':
            self.move_for_n(1)
            valuelist.append(self.parse_expr_or_string())
            rest = self.parse_comma_expr_list()
            if rest is not None:
                valuelist.extend(rest)
            return valuelist
        elif token.word == '}':
            return valuelist

    def parse_expr_or_string(self):
        token = self.get_current_token()
        if token.lexme_type == LexmeType.String:
            self.move_for_n(1)
            return ast.ConstantNode(self.context, token)
        else:
            return self.parse_expr()

    def parse_expr(self):
        return self.parse_logic_or()

    def parse_logic_or(self):
        left = self.parse_logic_and()
        rest = self.parse_lo()
        if rest is not None:
            return ast.BinaryExprNode(self.context, left, rest[0], rest[1])
        else:
            return left

    def parse_lo(self):
        token = self.get_current_token()
        if token.word == '||':
            self.move_for_n(1)
            left = self.parse_logic_and()
            rest = self.parse_lo()
            if rest is not None:
                node = ast.BinaryExprNode(self.context, left, rest[0], rest[1])
                return [token, node]
            else:
                return [token, left]
        else:
            return

    def parse_logic_and(self):
        left = self.parse_bit_or()
        rest = self.parse_la()
        if rest is not None:
            return ast.BinaryExprNode(self.context, left, rest[0], rest[1])
        else:
            return left

    def parse_la(self):
        token = self.get_current_token()
        if token.word == '&&':
            self.move_for_n(1)
            left = self.parse_bit_or()
            rest = self.parse_la()
            if rest is not None:
                node = ast.BinaryExprNode(self.context, left, rest[0], rest[1])
                return [token, node]
            else:
                return [token, left]
        else:
            return

    def parse_bit_or(self):
        left = self.parse_bit_and()
        rest = self.parse_bo()
        if rest is not None:
            return ast.BinaryExprNode(self.context, left, rest[0], rest[1])
        else:
            return left

    def parse_bo(self):
        token = self.get_current_token()
        if token.word == '|':
            self.move_for_n(1)
            left = self.parse_bit_and()
            rest = self.parse_bo()
            if rest is not None:
                node = ast.BinaryExprNode(self.context, left, rest[0], rest[1])
                return [token, node]
            else:
                return [token, left]
        else:
            return

    def parse_bit_and(self):
        left = self.parse_equal_or_not()
        rest = self.parse_ba()
        if rest is not None:
            return ast.BinaryExprNode(self.context, left, rest[0], rest[1])
        else:
            return left

    def parse_ba(self):
        token = self.get_current_token()
        if token.word == '&':
            self.move_for_n(1)
            left = self.parse_equal_or_not()
            rest = self.parse_ba()
            if rest is not None:
                node = ast.BinaryExprNode(self.context, left, rest[0], rest[1])
                return [token, node]
            else:
                return [token, left]
        else:
            return

    def parse_equal_or_not(self):
        left = self.parse_compare()
        rest = self.parse_eon()
        if rest is not None:
            return ast.BinaryExprNode(self.context, left, rest[0], rest[1])
        else:
            return left

    def parse_eon(self):
        token = self.get_current_token()
        if token.word == '==' or token.word == '!=':
            self.move_for_n(1)
            left = self.parse_compare()
            rest = self.parse_eon()
            if rest is not None:
                node = ast.BinaryExprNode(self.context, left, rest[0], rest[1])
                return [token, node]
            else:
                return [token, left]
        else:
            return

    def parse_compare(self):
        left = self.parse_bit_shift()
        rest = self.parse_c()
        if rest is not None:
            return ast.BinaryExprNode(self.context, left, rest[0], rest[1])
        else:
            return left

    def parse_c(self):
        token = self.get_current_token()
        if token.word == '>=' or token.word == '>' or token.word == '<' or token.word == '<=':
            self.move_for_n(1)
            left = self.parse_bit_shift()
            rest = self.parse_c()
            if rest is not None:
                node = ast.BinaryExprNode(self.context, left, rest[0], rest[1])
                return [token, node]
            else:
                return [token, left]
        else:
            return

    def parse_bit_shift(self):
        left = self.parse_plus_minus()
        rest = self.parse_bs()
        if rest is not None:
            return ast.BinaryExprNode(self.context, left, rest[0], rest[1])
        else:
            return left

    def parse_bs(self):
        token = self.get_current_token()
        if token.word == '>>' or token.word == '<<':
            self.move_for_n(1)
            left = self.parse_plus_minus()
            rest = self.parse_bs()
            if rest is not None:
                node = ast.BinaryExprNode(self.context, left, rest[0], rest[1])
                return [token, node]
            else:
                return [token, left]
        else:
            return

    def parse_plus_minus(self):
        left = self.parse_multi_divide()
        rest = self.parse_pm()
        if rest is not None:
            return ast.BinaryExprNode(self.context, left, rest[0], rest[1])
        else:
            return left

    def parse_pm(self):
        token = self.get_current_token()
        if token.word == '+' or token.word == '-':
            self.move_for_n(1)
            left = self.parse_multi_divide()
            rest = self.parse_pm()
            if rest is not None:
                node = ast.BinaryExprNode(self.context, left, rest[0], rest[1])
                return [token, node]
            else:
                return [token, left]
        else:
            return

    def parse_multi_divide(self):
        left = self.parse_factor()
        rest = self.parse_md()
        if rest is not None:
            return ast.BinaryExprNode(self.context, left, rest[0], rest[1])
        else:
            return left

    def parse_md(self):
        token = self.get_current_token()
        if token.word == '*' or token.word == '/' or token.word == '%':
            self.move_for_n(1)
            left = self.parse_factor()
            rest = self.parse_md()
            if rest is not None:
                node = ast.BinaryExprNode(self.context, left, rest[0], rest[1])
                return [token, node]
            else:
                return [token, left]
        else:
            return

    def parse_factor(self):
        token = self.get_current_token()
        if token.word == '!':
            self.move_for_n(1)
            return ast.SingleExprNode(self.context, token, self.parse_atom())
        elif token.word == '-':
            self.move_for_n(1)
            return ast.SingleExprNode(self.context, token, self.parse_atom())
        else:
            return self.parse_atom()

    def parse_atom(self):
        token = self.get_current_token()
        if token.lexme_type == LexmeType.Variable:
            if self.lookup_for_n(1).word == '[':
                self.move_for_n(2)
                index_expr_node = self.parse_expr()
                right_bracket = self.get_current_token()
                if right_bracket.word == ']':
                    return ast.ArrayVariableNode(self.context, token, index_expr_node)
                else:
                    raise cmexception.SyntaxException(right_bracket, [']'])
            elif self.lookup_for_n(1).word == '(':
                self.move_for_n(2)
                valuelist = self.parse_array_value_list()
                right_brace = self.get_current_token()
                if right_brace.word == ')':
                    self.move_for_n(1)
                    return ast.FunctionCallNode(self.context, token, valuelist)
                else:
                    raise cmexception.SyntaxException(right_brace, [')'])
            else:
                self.move_for_n(1)
                return ast.VariableNode(self.context, token)
        elif token.word == '(':
            self.move_for_n(1)
            expr_node = self.parse_expr()
            right_brace = self.get_current_token()
            if right_brace.word == ')':
                self.move_for_n(1)
                return expr_node
            else:
                raise cmexception.SyntaxException(right_brace, [')'])
        elif token.lexme_type == LexmeType.Double:
            self.move_for_n(1)
            return ast.ConstantNode(self.context, token)
        elif token.lexme_type == LexmeType.Integer:
            self.move_for_n(1)
            return ast.ConstantNode(self.context, token)
        elif token.lexme_type == LexmeType.Char:
            self.move_for_n(1)
            return ast.ConstantNode(self.context, token)
        else:
            raise cmexception.SyntaxException(token, ['<legal identifier>', '(', '<legal number>', '<ascii character>'])


class ASTContext:
    def __init__(self, parent_context):
        self.type_table = {}
        self.value_table = {}
        self.parent_context = parent_context
