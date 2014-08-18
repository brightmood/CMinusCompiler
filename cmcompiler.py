#coding=utf-8

import sys
from lexer import Lexer
from parser import Parser

if __name__ == '__main__':
    if len(sys.argv) >= 2:
        for filename in sys.argv[1:]:
            try:
                f = open(filename, "r")
                lexer = Lexer(f)
                # token = lexer.get_next_token()
                # while token:
                #     token = lexer.get_next_token()
                # f.close()
                parser = Parser(lexer)
                parser.process()
            except IOError, e:
                print "Can not find source file"
    else:
        print "No source file is specified"

