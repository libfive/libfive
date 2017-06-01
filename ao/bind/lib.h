#pragma once
#include <cstdint>

#ifdef __cplusplus
namespace Kernel {
    class Tree;
}

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
    uint32_t count;
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
#else
typedef void* ao_tree;
#endif

ao_tree ao_tree_x();
ao_tree ao_tree_y();
ao_tree ao_tree_z();

ao_tree ao_tree_const(float f);

ao_tree ao_tree_nonary(int op);
ao_tree ao_tree_unary(int op, ao_tree a);
ao_tree ao_tree_binary(int op, ao_tree a, ao_tree b);

float ao_tree_eval_f(ao_tree t, ao_vec3 p);

bool ao_tree_eq(ao_tree a, ao_tree b);

void ao_tree_delete(ao_tree ptr);

////////////////////////////////////////////////////////////////////////////////

/*
 *  Renders a tree to a set of contours
 *  The returned struct must be freed with ao_contours_delete
 */
ao_contours* ao_tree_render_slice(ao_tree tree, ao_region2 R,
                                  float z, float res);

/*
 *  Renders a tree to a set of triangles
 *  The returned struct must be freed with ao_mesh_delete
 */
ao_mesh* ao_tree_render_mesh(ao_tree tree, ao_region3 R, float res);

/*
 *  Renders and saves a mesh to a file
 *  Returns true on success, false otherwise
 */
bool ao_tree_save_mesh(ao_tree tree, ao_region3 R, float res, const char* f);

#ifdef __cplusplus
}
#endif
