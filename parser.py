# coding=utf-8


class Parser:

    def __init__(self, lexer):
        self.lexer = lexer
        self.llk = []
        self.llk.append(self.lexer.get_next_token())
        self.cursor = 0

    def lookup_for_n(self, n):
        time = n + 1 - self.cursor
        if time > 0:
            for x in range(1, time):
                self.llk.append(self.lexer.get_next_token())
        return self.llk[n].word

    def get_current_token(self):
        return self.llk[self.cursor].word

    def process(self):
        self.parse_program()

    def parse_program(self):
        self.parse_d()
        self.parse_e()

    def parse_e(self):
        token = self.get_current_token()
        if token == '':
            return
        else:
            self.parse_d()
            self.parse_e()

    def parse_d(self):
        token = self.lookup_for_n(2)
        if token == '(':
            self.parse_func_prototype()
            self.parse_s()
        else:
            self.parse_f()

    def parse_statement(self):
        pass

    def parse_f(self):
        pass

    def parse_func_prototype(self):
        pass

    def parse_s(self):
        pass

    def parse_var_decl(self):
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
