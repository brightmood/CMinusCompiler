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
        return "Syntax error at line %d column %d, " \
               "%s is not acceptable, expected '%s' ." \
               % (self.token.line_num, self.token.column_num, self.token.word, ','.join(self.expected))


class RedefineException(CMException):

    def __init__(self, token, extra=None):
        CMException.__init__(self, token)
        if extra is None:
            self.extra = 'identifier'
        else:
            self.extra = extra

    def to_string(self):
        return "Compile error at line: %d column: %d, " \
               "%s '%s' has been defined in current scope" \
               % (self.token.line_num, self.token.column_num, self.extra, self.token.word)


class NotDefinedException(CMException):

    def __init__(self, token, extra=None):
        CMException.__init__(self, token)
        if extra is None:
            self.extra = 'identifier'
        else:
            self.extra = extra

    def to_string(self):
        return "Compile error at line: %d column: %d, " \
               "%s '%s' has not been defined in current scope" \
               % (self.token.line_num, self.token.column_num, self.extra, self.token.word)


class ArrayIndexOutOfBoundException(CMException):

    def __init__(self, token, length):
        CMException.__init__(self, token)
        self.length = length

    def to_string(self):
        return "Array out of bound error at line: %d column: %d, " \
               "max index is %d but the given index is %d" \
               % (self.token.line_num, self.token.column_num, self.length-1, int(self.token.word))

