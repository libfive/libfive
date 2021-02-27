'''
Python bindings to the libfive CAD kernel
Copyright (C) 2021  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
'''

import ctypes
import numbers

from libfive.ffi import lib

class Shape:
    def __init__(self, ptr):
        ''' Builds a Shape from a raw pointer.

            It is unlikely that you want to call this by hand; consider using
            X/Y/Z or the @shape decorator instead.
        '''
        self.ptr = ptr

    def __del__(self):
        lib.libfive_tree_delete(self.ptr)

    @classmethod
    def new(cls, op, *args):
        if isinstance(op, str):
            op = op.encode('utf-8')
        if isinstance(op, bytes):
            op = lib.libfive_opcode_enum(op)
        if op == -1:
            raise RuntimeError("Invalid opcode")
        num_args = lib.libfive_opcode_args(op)
        if len(args) != num_args:
            raise RuntimeError("Error: opcode {} takes {} arguments"
                    .format(op, num_args))
        if num_args == 0:
            return cls(lib.libfive_tree_nullary(op))
        elif num_args == 1:
            return cls(lib.libfive_tree_unary(op, args[0]))
        elif num_args == 2:
            return cls(lib.libfive_tree_unary(op, args[1]))

    @classmethod
    def wrap(cls, t):
        ''' Ensures that the input argument is a Shape, converting constants
        '''
        if isinstance(t, numbers.Number):
            return cls(lib.libfive_tree_const(t))
        elif isinstance(t, cls):
            return t
        else:
            raise RuntimeError("Cannot convert {} into a Shape".format(t))

    @classmethod
    def X(cls):
        return cls.new('var-x')
    @classmethod
    def Y(cls):
        return cls.new('var-y')
    @classmethod
    def Z(cls):
        return cls.new('var-z')

    def __repr__(self):
        return "<shape@0x{}>".format(self.ptr)

    def __str__(self):
        s = lib.libfive_tree_print(self.ptr)
        try:
            return ctypes.c_char_p(s).value.decode('utf-8')
        finally:
            lib.libfive_free_str(ctypes.cast(s, ctypes.c_char_p))

