#coding=utf-8


class Parser:

    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = lexer.get_next_token()

    def process(self):
        pass


