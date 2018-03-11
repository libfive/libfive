/*
libfive: a CAD kernel for modeling with implicit functions
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
#include "libfive/tree/tree.hpp"
#include "libfive/tree/archive.hpp"

extern "C" {
#endif

struct libfive_interval  { float lower; float upper; };
struct libfive_region2   { libfive_interval X, Y; };
struct libfive_region3   { libfive_interval X, Y, Z; };

struct libfive_vec2      { float x, y; };
struct libfive_vec3      { float x, y, z; };
struct libfive_vec4      { float x, y, z, w; };
struct libfive_tri       { uint32_t a, b, c; };

struct libfive_contour {
    libfive_vec2* pts;
    uint32_t count;
};

struct libfive_contours {
    libfive_contour* cs;
    uint32_t count;
};

struct libfive_mesh {
    libfive_vec3* verts;
    libfive_tri* tris;
    uint32_t tri_count;
    uint32_t vert_count;
};

struct libfive_pixels {
    bool* pixels;
    uint32_t width;
    uint32_t height;
};

////////////////////////////////////////////////////////////////////////////////

/*
 *  Frees an libfive_contours data structure
 */
void libfive_contours_delete(libfive_contours* cs);

/*
 *  Frees an libfive_mesh data structure
 */
void libfive_mesh_delete(libfive_mesh* m);

/*
 *  Frees an libfive_pixels data structure
 */
void libfive_pixels_delete(libfive_pixels* ps);

/*
 *  Takes a string description of an op-code ('min', 'max', etc) and
 *  returns the Kernel::Opcode value, or -1 if no such value exists.
 */
int libfive_opcode_enum(const char* op);

/*
 *  Returns the number of arguments for the given opcode
 *  (either 0, 1, 2, or -1 if the opcode is invalid)
 */
int libfive_opcode_args(int op);

////////////////////////////////////////////////////////////////////////////////

#ifdef __cplusplus
typedef Kernel::Tree* libfive_tree;
typedef Kernel::Tree::Id libfive_id;
typedef Kernel::Archive* libfive_archive;
#else
typedef void* libfive_tree;
typedef void* libfive_id;
typedef void* libfive_archive;
#endif

libfive_tree libfive_tree_x();
libfive_tree libfive_tree_y();
libfive_tree libfive_tree_z();

libfive_tree libfive_tree_var();
bool libfive_tree_is_var(libfive_tree t);

libfive_tree libfive_tree_const(float f);
float libfive_tree_get_const(libfive_tree t, bool* success);

libfive_tree libfive_tree_constant_vars(libfive_tree t);

libfive_tree libfive_tree_nonary(int op);
libfive_tree libfive_tree_unary(int op, libfive_tree a);
libfive_tree libfive_tree_binary(int op, libfive_tree a, libfive_tree b);

const void* libfive_tree_id(libfive_tree t);

float libfive_tree_eval_f(libfive_tree t, libfive_vec3 p);
libfive_interval libfive_tree_eval_r(libfive_tree t, libfive_region3 r);
libfive_vec3 libfive_tree_eval_d(libfive_tree t, libfive_vec3 p);

bool libfive_tree_eq(libfive_tree a, libfive_tree b);

void libfive_tree_delete(libfive_tree ptr);

bool libfive_tree_save(libfive_tree ptr, const char* filename);
libfive_tree libfive_tree_load(const char* filename);

libfive_tree libfive_tree_remap(libfive_tree p,
        libfive_tree x, libfive_tree y, libfive_tree z);

libfive_region3 libfive_tree_bounds(libfive_tree p);

/*
 *  Returns a C string representing the tree in Scheme style
 *  (e.g. "(+ 1 2 x y)" )
 *
 *  The caller is responsible for freeing the string with free()
 */
char* libfive_tree_print(libfive_tree t);

/*
 *  Sweeps the given tree along a quadratic Bezier curve
 */
libfive_tree libfive_tree_sweep_quadratic(libfive_tree p,
    libfive_vec3 a, libfive_vec3 b, libfive_vec3 c);

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
 *  The returned struct must be freed with libfive_contours_delete
 */
libfive_contours* libfive_tree_render_slice(libfive_tree tree,
                                            libfive_region2 R,
                                            float z, float res);

/*
 *  Renders and saves a slice to a file
 *
 *  See argument details in libfive_tree_render_slice
 */
void libfive_tree_save_slice(libfive_tree tree, libfive_region2 R,
                             float z, float res, const char* f);

/*
 *  Renders a tree to a set of triangles
 *
 *  R is a region that will be subdivided into an octree.  For clean
 *  triangles, it should be near-cubical, but that isn't a hard requirement
 *
 *  res should be approximately half the model's smallest feature size;
 *  subdivision halts when all sides of the region are below it.
 *
 *  The returned struct must be freed with libfive_mesh_delete
 */
libfive_mesh* libfive_tree_render_mesh(libfive_tree tree,
                                       libfive_region3 R, float res);

/*
 *  Renders and saves a mesh to a file
 *
 *  Returns true on success, false otherwise
 *  See argument details in libfive_tree_render_mesh
 */
bool libfive_tree_save_mesh(libfive_tree tree, libfive_region3 R,
                            float res, const char* f);

/*
 *  Renders a 2D slice of pixels at the given Z height
 *
 *  The returned struct must be freed with libfive_pixels_delete
 */
libfive_pixels* libfive_tree_render_pixels(libfive_tree tree,
                                           libfive_region2 R,
                                           float z, float res);

/*
 *  Returns the human-readable tag associated with this build,
 *  or the empty string if there is no such tag
 */
const char* libfive_git_version(void);

/*
 *  Returns the 7-character git hash associated with this build,
 *  with a trailing '+' if there are local (uncommitted) modifications
 */
const char* libfive_git_revision(void);

/*
 *  Returns the name of the branch associated with this build
 */
const char* libfive_git_branch(void);

#ifdef __cplusplus
}
#endif
