/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#pragma once

#include <string>

class Store;
class Token;
class Tree;

extern "C"
{
    struct v4 { float x; float y; float z; float w; };

    Store* store_new();
    void store_delete(Store* ptr);

    int opcode_enum(char* op);

////////////////////////////////////////////////////////////////////////////////

    Token* token_x(Store* s);
    Token* token_y(Store* s);
    Token* token_z(Store* s);

    Token* token_const(Store* s, float f);

    Token* token_unary(Store* s, int op, Token* a);
    Token* token_binary(Store* s, int op, Token* a, Token* b);

    /*
     *  If t is an AFFINE token, stores its parameters in vec and returns 1;
     *  otherwise, returns 0
     */
    int token_affine_vec(Token* t, v4* vec);

////////////////////////////////////////////////////////////////////////////////

    Tree* tree_new(Store* store, Token* root);
    void tree_delete(Tree* ptr);

    float tree_eval_double(Tree* tree, float x, float y, float z);

    void tree_export_heightmap(Tree* tree, char* filename,
                               float xmin, float xmax,
                               float ymin, float ymax,
                               float zmin, float zmax, float res);

    void tree_export_mesh(Tree* tree, char* filename,
                          float xmin, float xmax,
                          float ymin, float ymax,
                          float zmin, float zmax, float res);

////////////////////////////////////////////////////////////////////////////////

    void window_show_tree(char* filename, char* name, Tree* tree);
    void window_watch_file(char* dir, char* file);
    void window_clear_frames();
    void window_set_callback(void (*callback)(const char*));
    void window_set_thread_init(void (*init)());

////////////////////////////////////////////////////////////////////////////////

    void ao_halt();
    void ao_run();

}
