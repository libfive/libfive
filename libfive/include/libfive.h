/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#ifdef __cplusplus
#include <cstdint>
#include "libfive/tree/tree.hpp"
#include "libfive/tree/archive.hpp"
#include "libfive/eval/evaluator.hpp"
extern "C" {
#else
#include <stdint.h>
#include <stdbool.h>
#endif

/*
 *  libfive_interval is a range used in interval arithmetic
 *  It usually represents either a spatial region (along a single axis)
 *  or a range that is guaranteed to contain a value.
 */
typedef struct libfive_interval  { float lower; float upper; } libfive_interval;

/*
 *  libfive_region2:  A 2D region
 */
typedef struct libfive_region2   { libfive_interval X, Y; } libfive_region2;

/*
 *  libfive_region3:  A 3D region
 */
typedef struct libfive_region3   { libfive_interval X, Y, Z; } libfive_region3;

/*
 *  libfive_vec2:  A 2D point or vector
 */
typedef struct libfive_vec2      { float x, y; } libfive_vec2;

/*
 *  libfive_vec3:  A 3D point or vector
 */
typedef struct libfive_vec3      { float x, y, z; } libfive_vec3;

/*
 *  libfive_vec4:  A 4D point or vector
 */
typedef struct libfive_vec4      { float x, y, z, w; } libfive_vec4;

/*
 *  libfive_tri:    A triangle, with corners stored as indices
 *  into a separate vertex array
 */
typedef struct libfive_tri       { uint32_t a, b, c; } libfive_tri;

/*
 *  libfive_contour is a single 2D contour, consisting of a sequence of
 *  2D points plus a count of how many points are stored
 */
typedef struct libfive_contour {
    libfive_vec2* pts;
    uint32_t count;
} libfive_contour;

/*
 *  libfive_contour is a set of 2D contours, consisting of multiple
 *  libfive_contour objects and a count of how many are stored
 */
typedef struct libfive_contours {
    libfive_contour* cs;
    uint32_t count;
} libfive_contours;

/*
 *  libfive_contour3 is a single 2D contour, consisting of a sequence of
 *  3D points plus a count of how many points are stored
 */
typedef struct libfive_contour3 {
    libfive_vec3* pts;
    uint32_t count;
} libfive_contour3;

/*
 *  libfive_contours3 is a set of 2D contours, consisting of multiple
 *  libfive_contour3 objects and a count of how many are stored
 */
typedef struct libfive_contours3 {
    libfive_contour3* cs;
    uint32_t count;
} libfive_contours3;

/*
 *  libfive_mesh is an indexed 3D mesh.
 *  There are vert_count vertices, and tri_count triangles.
 */
typedef struct libfive_mesh {
    libfive_vec3* verts;
    libfive_tri* tris;
    uint32_t tri_count;
    uint32_t vert_count;
} libfive_mesh;

/*
 *  libfive_mesh_coords is an indexed 3D mesh, similar to
 *  libfive_mesh, with sets of vertex indices separated by -1 instead
 *  of using triangle structs. There are vert_count vertices, and
 *  coord_index_count coordinate indices (including the -1s), for
 *  coord_index_count / 4 total triangles.
 */
typedef struct libfive_mesh_coords {
    libfive_vec3* verts;
    uint32_t vert_count;
    int32_t* coord_indices;
    uint32_t coord_index_count;
} libfive_mesh_coords;

/*
 *  libfive_pixels is a bitmap representing occupancy
 *  There are width * height pixels, in row-major order
 */
typedef struct libfive_pixels {
    bool* pixels;
    uint32_t width;
    uint32_t height;
} libfive_pixels;

////////////////////////////////////////////////////////////////////////////////

/*
 *  Frees an libfive_contours data structure
 */
void libfive_contours_delete(libfive_contours* cs);

/*
 *  Frees an libfive_contours data structure
 */
void libfive_contours3_delete(libfive_contours3* cs);

/*
 *  Frees an libfive_mesh data structure
 */
void libfive_mesh_delete(libfive_mesh* m);

/*
 *  Frees an libfive_mesh_coords data structure
 */
void libfive_mesh_coords_delete(libfive_mesh_coords* m);

/*
 *  Frees an libfive_pixels data structure
 */
void libfive_pixels_delete(libfive_pixels* ps);

/*
 *  Takes a string description of an op-code ('min', 'max', etc) and
 *  returns the libfive::Opcode value, or -1 if no such value exists.
 */
int libfive_opcode_enum(const char* op);

/*
 *  Returns the number of arguments for the given opcode
 *  (either 0, 1, 2, or -1 if the opcode is invalid)
 */
int libfive_opcode_args(int op);

////////////////////////////////////////////////////////////////////////////////

/*  libfive_vars is a general-purpose struct for mapping a set of
 *  free variables to their values.  The variables are opaque
 *  pointers, i.e. values returned with libfive_tree_id. */
typedef struct libfive_vars {
    void* const* vars;
    float* values;
    uint32_t size;
} libfive_vars;
void libfive_vars_delete(libfive_vars* j);

////////////////////////////////////////////////////////////////////////////////

#ifdef __cplusplus
typedef libfive::Tree* libfive_tree;
typedef libfive::Tree::Id libfive_id;
typedef libfive::Archive* libfive_archive;
typedef libfive::Evaluator *libfive_evaluator;
#else
typedef struct libfive_tree_ libfive_tree_;
typedef struct libfive_tree_* libfive_tree;

typedef struct libfive_id_ libfive_id_;
typedef struct libfive_id_* libfive_id;

typedef struct libfive_archive_ libfive_archive_;
typedef struct libfive_archive_* libfive_archive;

typedef struct libfive_evaluator_ libfive_evaluator_;
typedef struct libfive_evaluator_ *libfive_evaluator;
#endif

/*
 *  Constructs a new tree that returns the X coordinate
 */
libfive_tree libfive_tree_x();

/*
 *  Constructs a new tree that returns the Y coordinate
 */
libfive_tree libfive_tree_y();

/*
 *  Constructs a new tree that returns the Z coordinate
 */
libfive_tree libfive_tree_z();

/*
 *  Constructs a new tree that contains a free variable
 */
libfive_tree libfive_tree_var();

/*
 *  Returns true if the given tree is a free variable
 */
bool libfive_tree_is_var(libfive_tree t);

/*
 *  Constructs a new tree that contains the given constant value
 */
libfive_tree libfive_tree_const(float f);

/*
 *  If t is a constant value, returns that value and sets *success to true.
 *  Otherwise, sets success to false and returns 0.
 */
float libfive_tree_get_const(libfive_tree t, bool* success);

/*
 *  Wraps a tree in an operation that sets the derivatives with respect to
 *  all of its free variables to zero.
 */
libfive_tree libfive_tree_constant_vars(libfive_tree t);

/*
 *  Constructs a tree with the given no-argument opcode
 *  Returns NULL if the opcode is invalid.
 */
libfive_tree libfive_tree_nonary(int op);

/*
 *  Constructs a tree with the given one-argument opcode
 *  Returns NULL if the opcode or argument is invalid
 */
libfive_tree libfive_tree_unary(int op, libfive_tree a);

/*
 *  Constructs a tree with the given two-argument opcode
 *  Returns NULL if the opcode or arguments are invalid
 */
libfive_tree libfive_tree_binary(int op, libfive_tree a, libfive_tree b);

/*
 *  Returns a unique ID for the given tree.  This is post-deduplication,
 *  e.g. all constant tree of value 1.0 will return the same id.
 */
const void* libfive_tree_id(libfive_tree t);

/*
 *  Evaluates the given math tree at the given position.
 *  TODO:  Free variables are treated as zero
 */
float libfive_tree_eval_f(libfive_tree t, libfive_vec3 p);

/*
 *  Evaluates the given math tree over a spatial region, returning an interval
 *  that is guaranteed to contain the result
 *  TODO:  Free variables are treated as zero
 */
libfive_interval libfive_tree_eval_r(libfive_tree t, libfive_region3 r);

/*
 *  Evaluates the partial derivatives of a math tree at a specific point,
 *  with respect to x, y, z.
 */
libfive_vec3 libfive_tree_eval_d(libfive_tree t, libfive_vec3 p);

/*
 *  Checks whether two trees are equal, taking deduplication into account
 */
bool libfive_tree_eq(libfive_tree a, libfive_tree b);

/*
 *  Deletes a tree.  If binding in a higher-level language, call this in
 *  a destructor / finalizer to avoid leaking memory
 */
void libfive_tree_delete(libfive_tree ptr);

/*  Serializes the given tree to a file, return true on success.
 *  The file format is not archival, and may change without notice */
bool libfive_tree_save(libfive_tree ptr, const char* filename);

/*  Deserializes a tree from a file. */
libfive_tree libfive_tree_load(const char* filename);

/*  Executes the remapping operation returning a tree
 *  q(x, y, z) = p(x'(x, y, z), y'(x, y, z), z'(x, y, z)) */
libfive_tree libfive_tree_remap(libfive_tree p,
        libfive_tree x, libfive_tree y, libfive_tree z);

/*
 *  Returns a C string representing the tree in Scheme style
 *  (e.g. "(+ 1 2 x y)" )
 *
 *  The caller is responsible for freeing the string with free()
 */
char* libfive_tree_print(libfive_tree t);

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
 *  Renders a tree to a set of contours, similar to libfive_tree_render_slice,
 *  except the contours are 3D points (see the libfive_contour3 struct) above.
 */
libfive_contours3* libfive_tree_render_slice3(libfive_tree tree,
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
 *  Renders to an alternate mesh format, see description of
 *  libfive_mesh_coords above.  The returned struct must be freed with
 *  libfive_mesh_coords_delete.
 */
libfive_mesh_coords* libfive_tree_render_mesh_coords(libfive_tree tree,
                                                     libfive_region3 R,
                                                     float res);

/*
 *  Renders and saves a mesh to a file
 *
 *  Returns true on success, false otherwise
 *  See argument details in libfive_tree_render_mesh
 */
bool libfive_tree_save_mesh(libfive_tree tree, libfive_region3 R,
                            float res, const char* f);

/*
 *  Renders and saves a mesh to a file
 *
 *  Returns true on success, false otherwise
 *  Second argument is an evaluator
 *  See other argument details in libfive_tree_render_mesh
 */
bool libfive_evaluator_save_mesh(libfive_evaluator evaluator, libfive_region3 R,
                                   const char *f);


/*
 *  Renders and saves multiple meshes mesh to a file
 *
 *  Returns true on success, false otherwise
 *
 *  Arguments are equivalent to Studio's resolution and quality
 *  settings.  In particular quality is a value q such that we
 *  collapse cells when the QEF error is below 10**(-q)
 *
 *  trees is a null-terminated list (since libfive_tree is a pointer
 *  under the hood).
 */
bool libfive_tree_save_meshes(
        libfive_tree trees[], libfive_region3 R,
        float res, float quality, const char* f);

/*
 *  Renders a 2D slice of pixels at the given Z height
 *
 *  The returned struct must be freed with libfive_pixels_delete
 */
libfive_pixels* libfive_tree_render_pixels(libfive_tree tree,
                                           libfive_region2 R,
                                           float z, float res);

/*
 *  Constructs a new evaluator
 */
libfive_evaluator libfive_tree_evaluator(libfive_tree tree, libfive_vars vars);

/*
 *  Upates the variables of the evaluator
 */
bool libfive_evaluator_update_vars(libfive_evaluator eval_tree, libfive_vars vars);

/*
 *  Deletes (first) evaluator.  TODO: if settings.workers > 1
 */
void libfive_evaluator_delete(libfive_evaluator ptr);

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
