#pragma once
#include <cstdint>

namespace Kernel {
    class Tree;
    struct Contours;
    struct Mesh;
}

extern "C"
{

struct ao_interval { float lower; float upper; };
struct ao_vec2 { float x; float y; };
struct ao_vec3 { float x; float y; float z; };
struct ao_vec4 { float x; float y; float z; float w; };
struct ao_tri { uint32_t a; uint32_t b; uint32_t c; };

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
int ao_opcode_enum(char* op);

////////////////////////////////////////////////////////////////////////////////

typedef Kernel::Tree* ao_tree;

ao_tree ao_tree_x();
ao_tree ao_tree_y();
ao_tree ao_tree_z();

ao_tree ao_tree_const(float f);

ao_tree ao_tree_unary(int op, ao_tree a);
ao_tree ao_tree_binary(int op, ao_tree a, ao_tree b);

void ao_tree_delete(ao_tree ptr);

////////////////////////////////////////////////////////////////////////////////

ao_contours* ao_tree_render_slice(ao_tree tree,
        ao_interval x, ao_interval y, float z, float res);

ao_mesh* ao_tree_render_mesh(ao_tree tree,
        ao_interval x, ao_interval y, ao_interval z, float res);

}
