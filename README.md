CMinusCompiler
==============

A C-style compiler with llvmpy

How to use:
===========

1. Download LLVM3.4 http://www.llvm.org/releases/download.html#3.4 ,
then compile it. 
2. Install llvmpy as it's official site says. http://www.llvmpy.org/ .
3. Run python cmcompiler.py

Noticed syntax of this C-style language
===============================
Good news:</br>
    1. You can use functions, arrays, and global variable like C.</br>
    2. You can use type casting like C.</br>
    3. It supports four data types: char, int, double and String.</br>
    All of them is singed.</br>
    4. Bit computing and logic computing is supported.</br>
Bad news:</br>
    1. It doesn't support pointer and structs right now.</br>
    2. It only supports to compile a single c file right now.</br>
    3. It Lacks of pointers, so I add "String" type to support string, </br>
    something like java.</br>
    4. For loop is not supported.</br>
    The four points above are not going to be in my version 1.0. I will</br>
    try to put them in version 1.5.</br>

Progress
=========
    I am working on add built-in functions and while-loop, after which 
I will publish it as version 1.0.

