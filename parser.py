# coding=utf-8

from lexer import LexmeType


class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        self.llk = []
        self.llk.append(self.lexer.get_next_token())
        self.context = ASTContext(None)
    # self.cursor = 0

    def lookup_for_n(self, n):
        length = len(self.llk)
        if n > length - 1:
            for x in range(1, n + 1 - length):
                self.llk.append(self.lexer.get_next_token())
        return self.llk[n]

    def move_for_n(self, n):
        self.lookup_for_n(n)
        self.llk = self.llk[n:]
        return self.llk[0]

    def get_current_token(self):
        return self.llk[0]

    def process(self):
        self.parse_program()

    def parse_program(self):
        self.parse_d()
        self.parse_e()

    def parse_e(self):
        token = self.get_current_token()
        if token.word == '':
            return
        else:
            self.parse_d()
            self.parse_e()

    def parse_d(self):
        token = self.get_current_token()
        if token.lexmeType == LexmeType.Variable:
            self.parse_f()
        else:
            look_forward2 = self.lookup_for_n(2)
            if look_forward2.word == '(':
                self.parse_func_prototype()
                self.parse_s()
            else:
                self.parse_f()

    def parse_statement(self):
        pass

    def parse_f(self):
        token = self.get_current_token()
        if token.lexmeType == LexmeType.Variable:
            look_forward_1 = self.lookup_for_n(1)
            if look_forward_1.word == '[':
                self.parse_array_assign()
            elif look_forward_1.word == '=':
                self.parse_var_assign()
            else:
                self.print_error(look_forward_1, ['[', '='])
        elif token.word == 'int' or token.word == 'char' \
                or token.word == 'String' or token.word == 'double':
            look_forward_2 = self.lookup_for_n(2)
            if look_forward_2.word == ';':
                self.parse_var_decl()
            elif look_forward_2.word == '[':
                look_forward_5 = self.lookup_for_n(5)
                if look_forward_5 == ';':
                    self.parse_array_decl()
                else:
                    self.parse_array_decl_and_assign()
            elif look_forward_2.word == '=':
                self.parse_var_decl_and_assign()
            else:
                self.print_error(look_forward_2, [';', '[', '='])


    def parse_func_prototype(self):
        pass

    def parse_s(self):
        pass

    def parse_var_decl(self):
        token = self.move_for_n(1)
        if token.lexmeType == LexmeType.Variable:
            pass

    def parse_array_decl(self):
        pass

    def parse_var_assign(self):
        pass

    def parse_array_assign(self):
        pass

    def parse_var_decl_and_assign(self):
        pass

    def parse_array_decl_and_assign(self):
        pass

    def parse_expr(self):
        pass

    def parse_logic_or(self):
        pass

    def parse_lo(self):
        pass

    def parse_logic_and(self):
        pass

    def parse_la(self):
        pass

    def parse_bit_or(self):
        pass

    def parse_bo(self):
        pass

    def parse_bit_and(self):
        pass

    def parse_ba(self):
        pass

    def parse_equal_or_not(self):
        pass

    def parse_eon(self):
        pass

    def parse_compare(self):
        pass

    def parse_c(self):
        pass

    def parse_bit_shift(self):
        pass

    def parse_bs(self):
        pass

    def parse_plus_minus(self):
        pass

    def parse_pm(self):
        pass

    def parse_multi_divide(self):
        pass

    def parse_md(self):
        pass

    def parse_factor(self):
        pass

    def parse_atom(self):
        pass

    @staticmethod
    def print_error(lexme, expected):
        print "syntax error at line %d column %d expected %s"(lexme.line_num, lexme.column_num, expected)


class ASTContext:
    def __init__(self, parent_context):
        self.type_table = {}
        self.value_table = {}
        self.parent_context = parent_context
