'''
Python bindings to the libfive CAD kernel
Copyright (C) 2021  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
'''

import ctypes
import os
import sys

def try_link(folder, name):
    if sys.platform == "linux" or sys.platform == "linux2":
        suffix = '.so'
    elif sys.platform == "darwin":
        suffix = '.dylib'
    elif sys.platform == "win32":
        suffix = '.dll'
    path = os.path.join(folder, name + suffix)
    try:
        return ctypes.cdll.LoadLibrary(path)
    except OSError:
        return None

lib_paths = ["libfive/src", ""]
framework_dir = os.environ.get('LIBFIVE_FRAMEWORK_DIR')
if framework_dir:
    lib_paths.insert(0, framework_dir)
for p in lib_paths:
    lib = try_link(p, 'libfive')
    if lib is not None:
        break
if lib is None:
    raise RuntimeError("Could not find libfive library")

stdlib_paths = ["libfive/stdlib", ""]
framework_dir = os.environ.get('LIBFIVE_FRAMEWORK_DIR')
if framework_dir:
    stdlib_paths.insert(0, framework_dir)
for p in stdlib_paths:
    stdlib = try_link(p, 'libfive-stdlib')
    if stdlib is not None:
        break
if stdlib is None:
    raise RuntimeError("Could not find libfive standard library")

################################################################################

class libfive_interval_t(ctypes.Structure):
    _fields_ = [("lower", ctypes.c_float), ("upper", ctypes.c_float)]
class libfive_region_t(ctypes.Structure):
    _fields_ = [("X", libfive_interval_t),
                ("Y", libfive_interval_t),
                ("Z", libfive_interval_t)]
class libfive_vec3_t(ctypes.Structure):
    _fields_ = [("x", ctypes.c_float),
                ("y", ctypes.c_float),
                ("z", ctypes.c_float)]

libfive_tree = ctypes.c_void_p

lib.libfive_tree_delete.argtypes = [libfive_tree]

lib.libfive_tree_const.argtypes = [ctypes.c_float]
lib.libfive_tree_const.restype = libfive_tree

lib.libfive_opcode_enum.argtypes = [ctypes.c_char_p]
lib.libfive_opcode_enum.restype = ctypes.c_int

lib.libfive_tree_is_var.argtypes = [libfive_tree]
lib.libfive_tree_is_var.restype = ctypes.c_uint8

lib.libfive_opcode_args.argtypes = [ctypes.c_int]
lib.libfive_opcode_args.restype = ctypes.c_int

lib.libfive_tree_nullary.argtypes = [ctypes.c_int]
lib.libfive_tree_nullary.restype = libfive_tree

lib.libfive_tree_unary.argtypes = [ctypes.c_int, libfive_tree]
lib.libfive_tree_unary.restype = libfive_tree

lib.libfive_tree_binary.argtypes = [ctypes.c_int, libfive_tree, libfive_tree]
lib.libfive_tree_binary.restype = libfive_tree

lib.libfive_tree_id.argtypes = [libfive_tree]
lib.libfive_tree_id.restype = ctypes.c_void_p

lib.libfive_tree_remap.argtypes = [libfive_tree, libfive_tree, libfive_tree, libfive_tree]
lib.libfive_tree_remap.restype = libfive_tree

lib.libfive_tree_print.argtypes = [libfive_tree]
lib.libfive_tree_print.restype = ctypes.c_void_p # actually a c_char_p,
# but we don't want Python to auto-convert into a bytestring

lib.libfive_free_str.argtypes = [ctypes.c_char_p]

lib.libfive_tree_save_mesh.argtypes = [libfive_tree, libfive_region_t, ctypes.c_float, ctypes.c_char_p]
lib.libfive_tree_save_mesh.restype = ctypes.c_uint8

lib.libfive_tree_save_meshes.argtypes = [libfive_tree, libfive_region_t, ctypes.c_float, ctypes.c_float, ctypes.c_char_p]
lib.libfive_tree_save_meshes.restype = ctypes.c_uint8

lib.libfive_tree_save.argtypes = [libfive_tree, ctypes.c_char_p]
lib.libfive_tree_save.restype = ctypes.c_bool

lib.libfive_tree_load.argtypes = [ctypes.c_char_p]
lib.libfive_tree_load.restype = libfive_tree

lib.libfive_tree_eval_f.argtypes = [libfive_tree, libfive_vec3_t]
lib.libfive_tree_eval_f.restype = ctypes.c_float

lib.libfive_tree_eval_r.argtypes = [libfive_tree, libfive_region_t]
lib.libfive_tree_eval_r.restype = libfive_interval_t

lib.libfive_tree_eval_d.argtypes = [libfive_tree, libfive_vec3_t]
lib.libfive_tree_eval_d.restype = libfive_vec3_t

