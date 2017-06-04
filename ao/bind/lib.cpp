#include <iostream>
#include <fstream>

#include "lib.h"

#include "ao/tree/opcode.hpp"
#include "ao/tree/tree.hpp"
#include "ao/tree/template.hpp"
#include "ao/render/region.hpp"
#include "ao/format/contours.hpp"
#include "ao/format/mesh.hpp"

using namespace Kernel;

void ao_contours_delete(ao_contours* cs)
{
    for (unsigned i=0; i < cs->count; ++i)
    {
        delete [] cs->cs[i].pts;
    }
    delete [] cs->cs;
    delete cs;
}

void ao_mesh_delete(ao_mesh* m)
{
    delete [] m->verts;
    delete [] m->tris;
    delete m;
}

int ao_opcode_enum(const char* op)
{
    auto o = Opcode::from_str(op);
    return (o == Opcode::INVALID || o == Opcode::LAST_OP) ? -1 : o;
}

int ao_opcode_args(int op)
{
    return (op >= 0 && op < Opcode::LAST_OP)
        ? Opcode::args(Opcode::Opcode(op))
        : -1;
}

////////////////////////////////////////////////////////////////////////////////

ao_tree ao_tree_x() { return new Tree(Tree::X()); }
ao_tree ao_tree_y() { return new Tree(Tree::Y()); }
ao_tree ao_tree_z() { return new Tree(Tree::Z()); }

ao_tree ao_tree_const(float f) { return new Tree(f); }

ao_tree ao_tree_nonary(int op)
{
    return new Tree(Opcode::Opcode(op));
}

ao_tree ao_tree_unary(int op, ao_tree a)
{
    return new Tree(Opcode::Opcode(op), *a);
}
ao_tree ao_tree_binary(int op, ao_tree a, ao_tree b)
{
    return new Tree(Opcode::Opcode(op), *a, *b);
}

void ao_tree_delete(ao_tree ptr)
{
    delete ptr;
}

bool ao_tree_save(ao_tree ptr, const char* filename)
{
    auto data = ptr->serialize();
    std::ofstream out;
    out.open(filename, std::ios::out|std::ios::binary);
    if (out.is_open())
    {
        out.write((const char*)&data[0], data.size());
        return true;
    }
    else
    {
        fprintf(stderr, "ao_tree_save: could not open file");
        return false;
    }
}

ao_tree ao_tree_load(const char* filename)
{
    std::ifstream in;
    in.open(filename, std::ios::in|std::ios::binary|std::ios::ate);
    if (in.is_open())
    {
        std::vector<uint8_t> data;
        data.resize(in.tellg());

        in.seekg(0, std::ios::beg);
        in.read((char*)&data[0], data.size());

        auto t = Template::deserialize(data);
        return t.tree.id() ? new Tree(t.tree) : NULL;
    }
    else
    {
        fprintf(stderr, "ao_tree_load: could not open file");
        return nullptr;
    }
}

float ao_tree_eval_f(ao_tree t, ao_vec3 p)
{
    Evaluator e(*t);
    return e.eval({p.x, p.y, p.z});
}

bool ao_tree_eq(ao_tree a, ao_tree b)
{
    return *a == *b;
}

////////////////////////////////////////////////////////////////////////////////

ao_contours* ao_tree_render_slice(ao_tree tree,
        ao_region2 R, float z, float res)
{
    Region region({R.X.lower, R.X.upper}, {R.Y.lower, R.Y.upper}, {z,z}, res);
    auto cs = Contours::render(*tree, region);

    auto out = new ao_contours;
    out->count = cs.contours.size();
    out->cs = new ao_contour[out->count];

    size_t i=0;
    for (auto& c : cs.contours)
    {
        out->cs[i].count = c.size();
        out->cs[i].pts = new ao_vec2[c.size()];

        size_t j=0;
        for (auto& pt : c)
        {
            out->cs[i].pts[j++] = {pt.x(), pt.y()};
        }
    }

    return out;
}

ao_mesh* ao_tree_render_mesh(ao_tree tree, ao_region3 R, float res)
{
    Region region({R.X.lower, R.X.upper}, {R.Y.lower, R.Y.upper},
                  {R.Z.lower, R.Z.upper}, res);
    auto ms = Mesh::render(*tree, region);

    auto out = new ao_mesh;
    out->verts = new ao_vec3[ms.verts.size()];
    out->count = ms.tris.size();
    out->tris = new ao_tri[ms.tris.size()];

    size_t i;

    i=0;
    for (auto& v : ms.verts)
    {
        out->verts[i++] = {v.x(), v.y(), v.z()};
    }

    i=0;
    for (auto& t : ms.tris)
    {
        out->tris[i++] = {(uint32_t)t.x(), (uint32_t)t.y(), (uint32_t)t.z()};
    }

    return out;
}

bool ao_tree_save_mesh(ao_tree tree, ao_region3 R, float res, const char* f)
{
    Region region({R.X.lower, R.X.upper}, {R.Y.lower, R.Y.upper},
                  {R.Z.lower, R.Z.upper}, res);
    auto ms = Mesh::render(*tree, region);
    return ms.save(f);
}
