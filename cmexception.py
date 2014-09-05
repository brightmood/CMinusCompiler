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


class CastException(CMException):

    def __init__(self, token, target_type, real_type):
        CMException.__init__(self, token)
        self.target_type = target_type
        self.real_type = real_type

    def to_string(self):
        return "Cast error at line: %d column: %d. Can't cast from %s to %s" \
               % (self.token.line_num, self.token.column_num, self.real_type, self.target_type)


class InvalidOperandException(CMException):

    def __init__(self, token, left_type, right_type):
        CMException.__init__(self, token)
        self.left_type = left_type
        self.right_type = right_type

    def to_string(self):
        return "Invalid operation at line: %d column: %d. " \
               "Invalid operands to operator %s between type %s and type %s" \
               % (self.token.line_num, self.token.column_num, self.token.word,
                  self.left_type, self.right_type)


class FunctionReturnTypeNotMatchedException(CMException):

    def __init__(self, func_name_token, return_token, expected_return_type, actual_return_type):
        CMException.__init__(self, func_name_token)
        self.return_token = return_token
        self.expected_return_type = expected_return_type
        self.actual_return_type = actual_return_type

    def to_string(self):
        if self.return_token is not None:
            return "Return type of function '%s' not matched at line: %d column: %d. " \
                   "Expected %s but return %s" \
                   % (self.token.word, self.return_token.line_num, self.return_token.column_num,
                      self.expected_return_type, self.actual_return_type)
        else:
            return "Return type of function '%s' not matched. Expected %s but return %s" \
                   % (self.token.word, self.expected_return_type, self.actual_return_type)

#1 means that function doesn't exist
#2 means that arguments number doesn't match
#3 means that arguments type doesn't match