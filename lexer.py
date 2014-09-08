# coding=utf-8


# lexme enum type
def enum(**enums):
    return type('Enum', (), enums)


LexmeType = enum(Variable=1, Integer=2, Double=3, String=4,
                 Char=5, Operator=6, Reserve=7)

ReserveWord = ('char', 'int', 'double', 'void', 'if', 'else',
               'for', 'do', 'while', 'break', 'return', 'String', 'extern')

letters = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
           'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
                                                                       'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
           'K', 'L', 'M',
           'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']

digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

single_operators = ['>', '<', '+', '-', '*', '/', '%', '!', '&', '|']

terminate_operators = [',', ';', '[', ']', '(', ')', '{', '}']

double_operator = ['>=', '<=', '==', '&&', '||', '<<', '>>', '!=']


# compressed DFA state transform Table
# 'digits' did not contains 0
# 'letters' contains '_'
def generate_equal_type():
    equal_type_table = {}
    for i in letters:
        equal_type_table[i] = 'letters'
    equal_type_table['_'] = 'letters'
    for i in digits[1:]:
        equal_type_table[i] = 'digits'
    for i in single_operators:
        equal_type_table[i] = 'single_operators'
    for i in terminate_operators:
        equal_type_table[i] = 'terminate_operators'
    return equal_type_table


EqualTypeTable = generate_equal_type()


def build_transform_table():
    table = {}
    state_num = 17
    for i in range(0, state_num):
        table[i] = {}
    table[1]['letters'] = 2
    table[1]['single_operators'] = 3
    table[1]['digits'] = 7
    table[1]['0'] = 6
    table[1]['"'] = 10
    table[1]["'"] = 13
    table[1]['terminate_operators'] = 5
    table[1]['='] = 16

    table[2]['letters'] = 2
    table[2]['digits'] = 2
    table[2]['0'] = 2

    table[3]['single_operators'] = 4
    table[3]['='] = 4

    table[6]['.'] = 8

    table[7]['digits'] = 7
    table[7]['0'] = 7
    table[7]['.'] = 8

    table[8]['digits'] = 9
    table[8]['0'] = 9

    table[9]['digits'] = 9
    table[9]['0'] = 9

    table[10]['any'] = 11
    table[10]['"'] = 12

    table[11]['any'] = 11
    table[11]['"'] = 12

    table[13]["any"] = 14

    table[14]["'"] = 15

    table[16]['='] = 4
    return table


DFATransformTable = build_transform_table()
AcceptedState = {
    2: LexmeType.Variable,
    3: LexmeType.Operator,
    4: LexmeType.Operator,
    5: LexmeType.Operator,
    6: LexmeType.Integer,
    7: LexmeType.Integer,
    9: LexmeType.Double,
    12: LexmeType.String,
    15: LexmeType.Char,
    16: LexmeType.Operator
}


class Lexme:
    def __init__(self, word, line_num, column_num, lexme_type):
        self.word = word
        self.line_num = line_num
        self.column_num = column_num
        self.lexme_type = lexme_type

    def to_string(self):
        return "\"%s\" at line %d, column %d" % (self.word, self.line_num, self.column_num)

    def is_type(self):
        return self.word == 'int' or self.word == 'char' \
               or self.word == 'String' or self.word == 'double'


class Lexer:
    def __init__(self, source):
        self.source = source
        self.line_num = 0
        self.column_num = 0
        self.line = None

    def get_next_token(self):
        if not self.line or self.column_num == len(self.line) or self.line[self.column_num] == '\n':
            self.line = self.source.readline()
            self.line_num += 1
            self.column_num = 0
            if self.line == '':
                print "OK"
                return None
            elif self.line.strip(' \t\r\n') == '':
                self.line = ''
                return self.get_next_token()
        return self.dfa()

    def dfa(self):
        # dfa initiate state is 1
        state = 1
        length = 0

        for ch in self.line[self.column_num:]:
            if ch == ' ' or ch == '\t' or ch == '\r':
                self.column_num += 1
            else:
                break

        for ch in self.line[self.column_num:]:

            equal_type = ch
            if ch in EqualTypeTable:
                equal_type = EqualTypeTable[ch]

            next_state_dict = DFATransformTable[state]
            next_state = None
            if equal_type in next_state_dict:
                next_state = next_state_dict[equal_type]
            elif 'any' in next_state_dict:
                next_state = next_state_dict['any']
            else:
                next_state = 0
            if next_state == 0:
                return self.get_lexme(state, length)
            else:
                state = next_state
                length += 1
        return self.get_lexme(state, length)

    def get_lexme(self, state, length):

        if state in AcceptedState:
            lexme_type = AcceptedState[state]
            word = self.line[self.column_num:self.column_num + length]
            if lexme_type == LexmeType.Operator:
                if not word in double_operator and length == 2:
                    print "Not acceptable in line %d at column %d\n" % (self.line_num, self.column_num + length)
                    exit(1)
            if lexme_type == LexmeType.Variable:
                if word in ReserveWord:
                    lexme_type = LexmeType.Reserve
            lexme = Lexme(word, self.line_num, self.column_num, lexme_type)
            print lexme.to_string()
            self.column_num += length
            return lexme
        else:
            print "Not acceptable in line %d at column %d\n" % (self.line_num, self.column_num + length)
            exit(1)









