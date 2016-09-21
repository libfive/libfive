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

class Token;

extern "C"
{
    struct v2 { float lower; float upper; };
    struct v3 { float x; float y; float z; };
    struct v4 { float x; float y; float z; float w; };

////////////////////////////////////////////////////////////////////////////////

    struct contours {
        float** xs;
        float** ys;
        uint32_t* sizes;
        uint32_t size;
    };

    void contours_delete(struct contours* cs);

////////////////////////////////////////////////////////////////////////////////

    int opcode_enum(char* op);

////////////////////////////////////////////////////////////////////////////////

    struct axes {
        Token* x;
        Token* y;
        Token* z;
    };
    struct axes token_axes();

    Token* token_const(float f);

    Token* token_unary(int op, Token* a);
    Token* token_binary(int op, Token* a, Token* b);

    /*
     *  If t is an AFFINE token, stores its parameters in vec and returns 1;
     *  otherwise, returns 0
     */
    int token_affine_vec(Token* t, v4* vec);

////////////////////////////////////////////////////////////////////////////////

    void tree_delete(Token* ptr);

    /*
     *  Evaluates the tree at the given position
     *  Returns the result
     */
    float tree_eval_double(Token* tree, float x, float y, float z);

    /*
     *  Evaluates the tree on the given interval.
     *  Stores results in x
     */
    void tree_eval_interval(Token* tree, v2* x, v2* y, v2* z);

    /*
     *  Exports a heighmap to a .png image
     *
     *  The filename must end in ".png" (otherwise an assertion will occur)
     */
    void tree_export_heightmap(Token* tree, char* filename,
                               float xmin, float xmax,
                               float ymin, float ymax,
                               float zmin, float zmax, float res);

    /*
     *  Exports a mesh to a .stl file
     *
     *  The filename must end in ".stl" (otherwise an assertion will occur)
     */
    void tree_export_mesh(Token* tree, char* filename,
                          float xmin, float xmax,
                          float ymin, float ymax,
                          float zmin, float zmax, float res);

    /*
     *  Exports a set of contours to a .svg file
     *
     *  The filename must end in ".svg" (otherwise an assertion will occur)
     */
    void tree_export_slice(Token* tree, char* filename,
                           float xmin, float xmax, float ymin, float ymax,
                           float z, float res);

    /*
     *  Renders a 2D slice into a set of contours
     *
     *  The resulting struct must be freed with contours_delete
     */
    struct contours* tree_render_slice(Token* tree,
            float xmin, float xmax, float ymin, float ymax,
            float z, float res);

    /*
     *  Renders a mesh, returning the number of triangles.
     *  *out is malloced to tris * 3 verts per tri * 3 coords per vert
     *  and is a flat list of [x0, y0, z0, x1, y1, z1, x2, y2, z2, ...]
     */
    int tree_render_mesh(Token* tree, float** out,
                         float xmin, float xmax,
                         float ymin, float ymax,
                         float zmin, float zmax, float res);

////////////////////////////////////////////////////////////////////////////////

    void window_show_tree(char* filename, char* name, Token* tree);
    void window_watch_file(char* dir, char* file);
    void window_clear_frames();
    void window_set_callback(void (*callback)(const char*));
    void window_set_thread_init(void (*init)());

////////////////////////////////////////////////////////////////////////////////

    /*
     *  Inverts the matrix with rows x, y, z, [0 0 0 1]
     *  x, y, and z are modified in-place
     *  Returns 1 on success, 0 otherwise
     */
    void matrix_invert(v4* x, v4* y, v4* z);

////////////////////////////////////////////////////////////////////////////////

    void ao_halt();
    void ao_run();

}
