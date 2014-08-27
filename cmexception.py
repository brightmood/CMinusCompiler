#coding=utf-8


class CMException(Exception):
    def __init__(self, token):
        Exception.__init__(self)
        self.token = token

    def to_string(self):
        pass


class SyntaxException(CMException):
    def __init__(self, token, expected):
        Exception.__init__(self)
        self.token = token
        self.expected = expected

    def to_string(self):
        return "Syntax error at line %d column %d, %s is not acceptable, expected '%s' ." \
               % (self.token.line_num, self.token.column_num, self.token.word, ','.join(self.expected))


class RedefineException(CMException):
    def __init__(self, token, extra):
        CMException.__init__(self, token)
        self.extra = extra

    def to_string(self):
        return 'Compile error at line: %d column: %d, %s %s has been defined' \
               % (self.token.line_num, self.token.column_num, self.extra, self.token.word)



