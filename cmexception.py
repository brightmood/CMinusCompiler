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

    def __init__(self, func_name_token, expected_return_type, actual_return_type):
        CMException.__init__(self, func_name_token)
        self.expected_return_type = expected_return_type
        self.actual_return_type = actual_return_type

    def to_string(self):
        return "Return type of function '%s' not matched. Expected %s but return %s.\n" \
               "Notice: each branch should have a return statement if the return type" \
               "is not 'void'" % (self.token.word, self.expected_return_type, self.actual_return_type)


#errno = 1 means that function doesn't exist
#errno = 2 means that arguments number doesn't match
#errno = 3 means that arguments type doesn't match
class FunctionCallNotMatchedException(CMException):

    def __init__(self, func_name_token, expected_arg_list, actual_arg_list, errno, number):
        CMException.__init__(self, func_name_token)
        self.expected_arg_list = expected_arg_list
        self.actual_arg_list = actual_arg_list
        self.errno = errno
        self.number = number

    def to_string(self):
        if self.errno == 1:
            return "Function %s has not been declared" % self.token.word
        if self.errno == 2:
            return "Function %s need %d parameters but is called with %d parameters" \
                   % (self.token.word, len(self.expected_arg_list), len(self.actual_arg_list))
        if self.errno == 3:
            return "Function %s call error. The %dth parameter is of type %s but called with %s" \
                   % (self.token.word, self.number, self.expected_arg_list, self.actual_arg_list)


class GlobalStatementAssignException(CMException):

    def __init__(self, var_name_token):
        CMException.__init__(self, var_name_token)

    def to_string(self):

        return "Syntax error at line %d column %d, " \
               "Right value of global statement should be a constant" \
               % (self.token.line_num, self.token.column_num)