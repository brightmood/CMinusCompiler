CMinusCompiler
==============

A C-style compiler with llvmpy

Usage:
===========

1. Download LLVM3.4 http://www.llvm.org/releases/download.html#3.4 ,
then compile it. 
2. Install llvmpy as it's official site says. http://www.llvmpy.org/ .
3. Run python cmcompiler.py

Noticed syntax of this C-style language
===============================

Good news:
-----------

1. You can use functions, arrays, and global variable like C.
2. You can use type casting like C.
3. It supports four data types: char, int, double and String. All of them is signed.
4. Bit computing and logic computing is supported.

Bad news:
----------

1. It doesn't support pointer and structs right now.
2. It only supports to compile a single c file right now.
3. It Lacks of pointers, so I add "String" type to support string, something like java.
4. For loop is not supported.
The four points above are not going to be in my version 1.0. I will
try to put them in version 1.5.

Progress
=========
    Version1.0. I will try to add pointer in next subversion

