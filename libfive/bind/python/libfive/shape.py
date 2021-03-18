'''
Python bindings to the libfive CAD kernel
Copyright (C) 2021  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
'''

import ctypes
import numbers
import tempfile
import subprocess

from libfive.ffi import lib, libfive_region_t, libfive_interval_t

def _wrapped(f):
    ''' Decorator function which calls Shape.wrap on every argument
        in f(self, *args), then calls f as usual
    '''
    def g(*args):
        args = [Shape.wrap(a) for a in args]
        return f(*args)
    return g

class Shape:
    def __init__(self, ptr):
        ''' Builds a Shape from a raw pointer.

            It is unlikely that you want to call this by hand; consider using
            X/Y/Z, the @shape decorator, or functions in libfive.stdlib instead
        '''
        self.ptr = ptr

    def __del__(self):
        lib.libfive_tree_delete(self.ptr)

    @classmethod
    def new(cls, op, *args):
        if isinstance(op, str):
            op = op.encode('ascii')
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
            return cls(lib.libfive_tree_binary(op, args[0], args[1]))

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
        return "<shape@0x{:x}>".format(self.ptr)

    def __str__(self):
        s = lib.libfive_tree_print(self.ptr)
        try:
            return ctypes.c_char_p(s).value.decode('ascii')
        finally:
            lib.libfive_free_str(ctypes.cast(s, ctypes.c_char_p))

    def __bool__(self):
        raise RuntimeError("Cannot check truthiness of Shape")

    @_wrapped
    def __add__(self, other):
        return Shape.new('add', self.ptr, other.ptr)

    @_wrapped
    def __sub__(self, other):
        return Shape.new('sub', self.ptr, other.ptr)

    @_wrapped
    def __mul__(self, other):
        return Shape.new('mul', self.ptr, other.ptr)

    @_wrapped
    def __mod__(self, other):
        return Shape.new('mod', self.ptr, other.ptr)

    @_wrapped
    def __pow__(self, other):
        return Shape.new('pow', self.ptr, other.ptr)

    @_wrapped
    def __truediv__(self, other):
        return Shape.new('div', self.ptr, other.ptr)

    def __eq__(self, other):
        raise RuntimeError("Shape does not support equality comparisons")

    @classmethod
    def var(cls):
        return cls.new('var-free')

    def with_constant_vars(self):
        return Shape.new('const-var', self.ptr)

    def lock(self):
        return self.with_constant_vars()

    def optimized(self):
        return Shape(lib.libfive_tree_optimized(self.ptr));

    def sqrt(self):
        return Shape.new('sqrt', self.ptr)

    def pow(self):
        return Shape.new('pow', self.ptr)

    def save_stl(self, filename, xyz_min=(-10,-10,-10), xyz_max=(10,10,10),
                 resolution=10):
        ''' Converts this Shape into a mesh and saves it as an STL file.

            xyz_min/max are three-element lists of corner positions
            resolution is the reciprocal of minimum feature size
                (larger values = higher resolution = slower)
        '''
        region = libfive_region_t(*[libfive_interval_t(a, b) for a, b
                                    in zip(xyz_min, xyz_max)])
        lib.libfive_tree_save_mesh(self.ptr, region, resolution,
                                   filename.encode('ascii'))

    def show(self, xyz_min=(-10,-10,-10), xyz_max=(10,10,10), resolution=10):
        with open('.out.stl','wb') as f:
            self.save_stl(f.name, xyz_min, xyz_max, resolution)
            subprocess.run(['open', f.name])

################################################################################

def shape(f):
    ''' Decorator which converts a target function into a Shape
    '''
    return f(Shape.X(), Shape.Y(), Shape.Z())

# Hotpatch every transform (which take a shape as their first arguments)
# into the Shape as methods, to make things easier to chain
import libfive.stdlib.transforms as _transforms
for (_name, _f) in _transforms.__dict__.items():
    if callable(_f):
        setattr(Shape, _name, _f)

# Hot-patch a few CSG functions onto the Shape class
import libfive.stdlib.csg as _csg
for _name in ['union', 'intersection', 'difference']:
    setattr(Shape, _name, getattr(_csg, _name))
