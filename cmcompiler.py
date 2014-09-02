#coding=utf-8

import sys
from lexer import Lexer
from parser import Parser
from cmexception import CMException

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
                ast_tree = parser.process()
                f.close()
                module = ast_tree.code_gen()
                print module
            except IOError, e:
                print "Can not find source file"
            except CMException, cme:
                print cme.to_string()
    else:
        print "No source file is specified"

