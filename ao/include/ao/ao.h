/*
Ao: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#pragma once
#include <cstdint>

#ifdef __cplusplus
#include "ao/tree/tree.hpp"
#include "ao/tree/template.hpp"

extern "C" {
#endif

struct ao_interval  { float lower; float upper; };
struct ao_region2   { ao_interval X; ao_interval Y; };
struct ao_region3   { ao_interval X; ao_interval Y; ao_interval Z; };

struct ao_vec2      { float x; float y; };
struct ao_vec3      { float x; float y; float z; };
struct ao_vec4      { float x; float y; float z; float w; };
struct ao_tri       { uint32_t a; uint32_t b; uint32_t c; };

struct ao_contour {
    ao_vec2* pts;
    uint32_t count;
};

struct ao_contours {
    ao_contour* cs;
    uint32_t count;
};

struct ao_mesh {
    ao_vec3* verts;
    ao_tri* tris;
    uint32_t tri_count;
    uint32_t vert_count;
};

struct ao_pixels {
    bool* pixels;
    uint32_t width;
    uint32_t height;
};

////////////////////////////////////////////////////////////////////////////////

/*
 *  Frees an ao_contours data structure
 */
void ao_contours_delete(ao_contours* cs);

/*
 *  Frees an ao_mesh data structure
 */
void ao_mesh_delete(ao_mesh* m);

/*
 *  Frees an ao_pixels data structure
 */
void ao_pixels_delete(ao_pixels* ps);

/*
 *  Takes a string description of an op-code ('min', 'max', etc) and
 *  returns the Kernel::Opcode value, or -1 if no such value exists.
 */
int ao_opcode_enum(const char* op);

/*
 *  Returns the number of arguments for the given opcode
 *  (either 0, 1, 2, or -1 if the opcode is invalid)
 */
int ao_opcode_args(int op);

////////////////////////////////////////////////////////////////////////////////

#ifdef __cplusplus
typedef Kernel::Tree* ao_tree;
typedef Kernel::Tree::Id ao_id;
typedef Kernel::Template* ao_template;
#else
typedef void* ao_tree;
typedef void* ao_id;
typedef void* ao_template;
#endif

ao_tree ao_tree_x();
ao_tree ao_tree_y();
ao_tree ao_tree_z();

ao_tree ao_tree_var();
bool ao_tree_is_var(ao_tree t);

ao_tree ao_tree_const(float f);
float ao_tree_get_const(ao_tree t, bool* success);

ao_tree ao_tree_nonary(int op);
ao_tree ao_tree_unary(int op, ao_tree a);
ao_tree ao_tree_binary(int op, ao_tree a, ao_tree b);

const void* ao_tree_id(ao_tree t);

float ao_tree_eval_f(ao_tree t, ao_vec3 p);
ao_interval ao_tree_eval_r(ao_tree t, ao_region3 r);

bool ao_tree_eq(ao_tree a, ao_tree b);

void ao_tree_delete(ao_tree ptr);

bool ao_tree_save(ao_tree ptr, const char* filename);
ao_tree ao_tree_load(const char* filename);

ao_tree ao_tree_remap(ao_tree p, ao_tree x, ao_tree y, ao_tree z);

////////////////////////////////////////////////////////////////////////////////

struct ao_args;
void ao_set_arg_name(ao_args* a, uint32_t i, const char* name);
void ao_args_delete(ao_args* a);
ao_args* ao_args_new(uint32_t count);
const char* ao_arg_name(ao_args* a, uint32_t i);
void ao_set_arg_name(ao_args* a, uint32_t i, const char* name);
void ao_set_arg_id(ao_args* a, uint32_t i, ao_id id);

ao_template ao_tree_to_template(ao_tree t);
ao_args* ao_template_args(ao_template t);

////////////////////////////////////////////////////////////////////////////////

/*
 *  Renders a tree to a set of contours
 *
 *  R is a region that will be subdivided into an octree.  For clean
 *  triangles, it should be near-cubical, but that isn't a hard requirement
 *
 *  res should be approximately half the model's smallest feature size;
 *  subdivision halts when all sides of the region are below it.
 *
 *  The returned struct must be freed with ao_contours_delete
 */
ao_contours* ao_tree_render_slice(ao_tree tree, ao_region2 R,
                                  float z, float res);

/*
 *  Renders and saves a slice to a file
 *
 *  See argument details in ao_tree_render_slice
 */
void ao_tree_save_slice(ao_tree tree, ao_region2 R, float z, float res,
                        const char* f);

/*
 *  Renders a tree to a set of triangles
 *
 *  R is a region that will be subdivided into an octree.  For clean
 *  triangles, it should be near-cubical, but that isn't a hard requirement
 *
 *  res should be approximately half the model's smallest feature size;
 *  subdivision halts when all sides of the region are below it.
 *
 *  The returned struct must be freed with ao_mesh_delete
 */
ao_mesh* ao_tree_render_mesh(ao_tree tree, ao_region3 R, float res);

/*
 *  Renders and saves a mesh to a file
 *
 *  Returns true on success, false otherwise
 *  See argument details in ao_tree_render_mesh
 */
bool ao_tree_save_mesh(ao_tree tree, ao_region3 R, float res, const char* f);

/*
 *  Renders a 2D slice of pixels at the given Z height
 *
 *  The returned struct must be freed with ao_pixels_delete
 */
ao_pixels* ao_tree_render_pixels(ao_tree tree, ao_region2 R,
                                 float z, float res);

#ifdef __cplusplus
}
#endif
