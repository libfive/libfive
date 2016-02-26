/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of Ao.
 *
 *  Ao is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#pragma once

#include <string>

class Store;
class Token;
class Tree;

extern "C"
{
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

    extern void (*window_watch_callback)(const char*);

////////////////////////////////////////////////////////////////////////////////

    void ao_halt();
    void ao_run();

}
