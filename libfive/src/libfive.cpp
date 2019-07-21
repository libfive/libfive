/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <iostream>
#include <fstream>

#include "libfive.h"

#include "libfive/tree/opcode.hpp"
#include "libfive/tree/tree.hpp"

#include "libfive/eval/eval_deriv_array.hpp"

#include "libfive/render/brep/region.hpp"
#include "libfive/render/brep/contours.hpp"
#include "libfive/render/brep/mesh.hpp"
#include "libfive/render/brep/settings.hpp"

#include "libfive/render/discrete/voxels.hpp"
#include "libfive/render/discrete/heightmap.hpp"

using namespace libfive;

void libfive_contours_delete(libfive_contours* cs)
{
    for (unsigned i=0; i < cs->count; ++i)
    {
        delete [] cs->cs[i].pts;
    }
    delete [] cs->cs;
    delete cs;
}

void libfive_contours3_delete(libfive_contours3* cs)
{
    for (unsigned i=0; i < cs->count; ++i)
    {
        delete [] cs->cs[i].pts;
    }
    delete [] cs->cs;
    delete cs;
}

void libfive_mesh_delete(libfive_mesh* m)
{
    delete [] m->verts;
    delete [] m->tris;
    delete m;
}

void libfive_mesh_coords_delete(libfive_mesh_coords* m)
{
    delete [] m->verts;
    delete [] m->coord_indices;
    delete m;
}

void libfive_pixels_delete(libfive_pixels* m)
{
    delete [] m->pixels;
    delete m;
}

void libfive_vars_delete(libfive_vars* vs)
{
    delete [] vs->vars;
    delete [] vs->values;
    delete vs;
}

int libfive_opcode_enum(const char* op)
{
    auto o = Opcode::fromScmString(op);
    return (o == Opcode::INVALID || o == Opcode::LAST_OP) ? -1 : o;
}

int libfive_opcode_args(int op)
{
    return (op >= 0 && op < Opcode::LAST_OP)
        ? Opcode::args(Opcode::Opcode(op))
        : -1;
}

////////////////////////////////////////////////////////////////////////////////

libfive_tree libfive_tree_x() { return new Tree(Tree::X()); }
libfive_tree libfive_tree_y() { return new Tree(Tree::Y()); }
libfive_tree libfive_tree_z() { return new Tree(Tree::Z()); }

libfive_tree libfive_tree_const(float f) { return new Tree(f); }
libfive_tree libfive_tree_var() { return new Tree(Tree::var()); }

bool libfive_tree_is_var(libfive_tree t)
{
    return (*t)->op == Opcode::VAR_FREE;
}

float libfive_tree_get_const(libfive_tree t, bool* success)
{
    if ((*t)->op == Opcode::CONSTANT)
    {
        if (success) { *success = true; }
        return (*t)->value;
    }
    if (success) { *success = false; }
    return 0;
}

libfive_tree libfive_tree_constant_vars(libfive_tree t)
{
    return new Tree(t->makeVarsConstant());
}

static bool opcode_is_valid(int op, size_t expected_args)
{
    return op >= 0 &&
           op < Opcode::LAST_OP &&
           Opcode::args(Opcode::Opcode(op)) == expected_args;
}

libfive_tree libfive_tree_nonary(int op)
{
    return opcode_is_valid(op, 0)
        ? new Tree(Opcode::Opcode(op))
        : nullptr;
}

libfive_tree libfive_tree_unary(int op, libfive_tree a)
{
    return (opcode_is_valid(op, 1) && a != nullptr)
        ? new Tree(Opcode::Opcode(op), *a)
        : nullptr;
}
libfive_tree libfive_tree_binary(int op, libfive_tree a, libfive_tree b)
{
    return (opcode_is_valid(op, 2) && a != nullptr && b != nullptr)
        ? new Tree(Opcode::Opcode(op), *a, *b)
        : nullptr;
}

const void* libfive_tree_id(libfive_tree t)
{
    return static_cast<const void*>(t->id());
}

void libfive_tree_delete(libfive_tree ptr)
{
    delete ptr;
}

bool libfive_tree_save(libfive_tree ptr, const char* filename)
{
    std::ofstream out;
    out.open(filename, std::ios::out|std::ios::binary);
    if (out.is_open())
    {
        ptr->serialize(out);
        return true;
    }
    else
    {
        std::cerr << "libfive_tree_save: could not open file" << std::endl;
        return false;
    }
}

libfive_tree libfive_tree_load(const char* filename)
{
    auto t = Tree::load(filename);
    if (t.id())
    {
        return new Tree(t);
    }
    else
    {
        std::cerr <<  "libfive_tree_load: could not open file" << std::endl;
        return nullptr;
    }
}

libfive_tree libfive_tree_remap(libfive_tree p, libfive_tree x, libfive_tree y, libfive_tree z)
{
    return new Tree(p->remap(*x, *y, *z));
}

float libfive_tree_eval_f(libfive_tree t, libfive_vec3 p)
{
    ArrayEvaluator e(*t);
    return e.value({p.x, p.y, p.z});
}

libfive_interval libfive_tree_eval_r(libfive_tree t, libfive_region3 r)
{
    IntervalEvaluator e(*t);
    auto i = e.eval({r.X.lower, r.Y.lower, r.Z.lower},
                    {r.X.upper, r.Y.upper, r.Z.upper});
    return {i.lower(), i.upper()};
}

libfive_vec3 libfive_tree_eval_d(libfive_tree t, libfive_vec3 p)
{
    DerivArrayEvaluator e(*t);
    auto v = e.deriv({p.x, p.y, p.z});
    return {v.x(), v.y(), v.z()};
}

bool libfive_tree_eq(libfive_tree a, libfive_tree b)
{
    return *a == *b;
}

char* libfive_tree_print(libfive_tree t)
{
    std::stringstream ss;
    (*t)->print(ss);
    const auto str = ss.str();

    auto out = static_cast<char*>(malloc(str.size() + 1 * sizeof(char)));
    memcpy(out, str.c_str(), str.size() + 1);
    return out;
}

////////////////////////////////////////////////////////////////////////////////

libfive_contours* libfive_tree_render_slice(libfive_tree tree,
        libfive_region2 R, float z, float res)
{
    Region<2> region({R.X.lower, R.Y.lower}, {R.X.upper, R.Y.upper},
            Region<2>::Perp(z));
    BRepSettings settings;
    settings.min_feature = 1/res;
    auto cs = Contours::render(*tree, region, settings);

    auto out = new libfive_contours;
    out->count = cs->contours.size();
    out->cs = new libfive_contour[out->count];

    size_t i=0;
    for (auto& c : cs->contours)
    {
        out->cs[i].count = c.size();
        out->cs[i].pts = new libfive_vec2[c.size()];

        size_t j=0;
        for (auto& pt : c)
        {
            out->cs[i].pts[j++] = {pt.x(), pt.y()};
        }
        i++;
    }

    return out;
}

libfive_contours3* libfive_tree_render_slice3(libfive_tree tree,
                                              libfive_region2 R, float z, float res)
{
    Region<2> region({R.X.lower, R.Y.lower}, {R.X.upper, R.Y.upper},
            Region<2>::Perp(z));
    BRepSettings settings;
    settings.min_feature = 1/res;
    auto cs = Contours::render(*tree, region, settings);

    auto out = new libfive_contours3;
    out->count = cs->contours.size();
    out->cs = new libfive_contour3[out->count];

    size_t i=0;
    for (auto& c : cs->contours)
    {
        out->cs[i].count = c.size();
        out->cs[i].pts = new libfive_vec3[c.size()];

        size_t j=0;
        for (auto& pt : c)
        {
          // each 2D contour point is converted to a 3D point (with
          // this function's z argument as the Z coordinate)
          out->cs[i].pts[j++] = {pt.x(), pt.y(), z};
        }
        i++;
    }

    return out;
}

void libfive_tree_save_slice(libfive_tree tree, libfive_region2 R, float z, float res,
                        const char* f)
{
    Region<2> region({R.X.lower, R.Y.lower}, {R.X.upper, R.Y.upper},
            Region<2>::Perp(z));
    BRepSettings settings;
    settings.min_feature = 1/res;
    auto cs = Contours::render(*tree, region, settings);
    cs->saveSVG(f);
}

libfive_mesh* libfive_tree_render_mesh(libfive_tree tree, libfive_region3 R, float res)
{
    Region<3> region({R.X.lower, R.Y.lower, R.Z.lower},
                     {R.X.upper, R.Y.upper, R.Z.upper});
    BRepSettings settings;
    settings.min_feature = 1/res;
    auto ms = Mesh::render(*tree, region, settings);
    if (ms.get() == nullptr)
    {
        fprintf(stderr, "libfive_tree_render_mesh: got empty mesh\n");
        return nullptr;
    }

    auto out = new libfive_mesh;
    out->verts = new libfive_vec3[ms->verts.size()];
    out->vert_count = ms->verts.size();
    out->tris = new libfive_tri[ms->branes.size()];
    out->tri_count = ms->branes.size();

    size_t i;

    i=0;
    for (auto& v : ms->verts)
    {
        out->verts[i++] = {v.x(), v.y(), v.z()};
    }

    i=0;
    for (auto& t : ms->branes)
    {
        out->tris[i++] = {(uint32_t)t.x(), (uint32_t)t.y(), (uint32_t)t.z()};
    }

    return out;
}

libfive_mesh_coords* libfive_tree_render_mesh_coords(libfive_tree tree,
                                                     libfive_region3 R,
                                                     float res)
{
    Region<3> region({R.X.lower, R.Y.lower, R.Z.lower},
                     {R.X.upper, R.Y.upper, R.Z.upper});
    BRepSettings settings;
    settings.min_feature = 1/res;
    auto ms = Mesh::render(*tree, region, settings);
    if (ms.get() == nullptr)
    {
        fprintf(stderr, "libfive_tree_render_mesh_coords: got empty mesh\n");
        return nullptr;
    }

    auto out = new libfive_mesh_coords;
    out->verts = new libfive_vec3[ms->verts.size()];
    out->vert_count = ms->verts.size();
    // need 4 times the count of triangles for coordinate indices
    // (3 vertices separated by -1 for each triangle)
    out->coord_indices = new int32_t[4 * ms->branes.size()];
    out->coord_index_count = 4 * ms->branes.size();

    size_t i;

    i=0;
    for (auto& v : ms->verts)
    {
        out->verts[i++] = {v.x(), v.y(), v.z()};
    }

    i=0;
    for (auto& t : ms->branes)
    {
      out->coord_indices[i++] = (int32_t)t.x();
      out->coord_indices[i++] = (int32_t)t.y();
      out->coord_indices[i++] = (int32_t)t.z();
      out->coord_indices[i++] = -1;
    }

    return out;
}

bool libfive_tree_save_mesh(libfive_tree tree, libfive_region3 R, float res, const char* f)
{
    Region<3> region({R.X.lower, R.Y.lower, R.Z.lower},
                     {R.X.upper, R.Y.upper, R.Z.upper});

    BRepSettings settings;
    settings.min_feature = 1/res;
    auto ms = Mesh::render(*tree, region, settings);
    return ms->saveSTL(f);
}

bool libfive_evaluator_save_mesh(libfive_evaluator evaluator, libfive_region3 R, const char *f)
{
    Region<3> region({R.X.lower, R.Y.lower, R.Z.lower},
                     {R.X.upper, R.Y.upper, R.Z.upper});

    BRepSettings settings; // TODO: pass it in as an argument
    settings.workers = 1;  // MOTE: temporay limitation
    auto ms = Mesh::render(evaluator, region, settings);
    return ms->saveSTL(f);
}

bool libfive_tree_save_meshes(
        libfive_tree trees[], libfive_region3 R,
        float res, float quality, const char* f)
{
    Region<3> region({R.X.lower, R.Y.lower, R.Z.lower},
                     {R.X.upper, R.Y.upper, R.Z.upper});

    BRepSettings settings;
    settings.min_feature = 1/res;
    settings.max_err = pow(10, -quality);
    std::list<const libfive::Mesh*> meshes;
    for (unsigned i=0; trees[i] != nullptr; ++i){
        auto ms = Mesh::render(*trees[i], region, settings);
        meshes.push_back(ms.release());
    }

    const bool out = Mesh::saveSTL(f, meshes);
    for (auto& m : meshes) {
        delete m;
    }
    return out;
}

libfive_pixels* libfive_tree_render_pixels(libfive_tree tree, libfive_region2 R,
                                 float z, float res)
{
    Voxels v({R.X.lower, R.Y.lower, z},
             {R.X.upper, R.Y.upper, z}, res);
    std::atomic_bool abort(false);
    auto h = Heightmap::render(*tree, v, abort);

    libfive_pixels* out = new libfive_pixels;
    out->width = h->depth.cols();
    out->height = h->depth.rows();
    out->pixels = new bool[out->width * out->height];

    size_t i=0;
    for (unsigned y=0; y < out->height; ++y)
    {
        for (unsigned x=0; x < out->width; ++x)
        {
            out->pixels[i++] = !std::isinf(h->depth(y, x));
        }
    }

    return out;
}

libfive_evaluator libfive_tree_evaluator(libfive_tree tree, libfive_vars vars)
{
    std::map<libfive::Tree::Id, float> mapOfVars;
    for (unsigned i = 0; i < vars.size; ++i)
    {
        auto treeId = static_cast<libfive::Tree::Id>(vars.vars[i]);
        mapOfVars.insert(std::make_pair(treeId, vars.values[i]));
    }
    // TODO: For more than one worker
    return new Evaluator(*tree, mapOfVars);
}

bool libfive_evaluator_update_vars(libfive_evaluator eval_tree, libfive_vars vars)
{
    std::map<libfive::Tree::Id, float> mapOfVars;
    for (unsigned i = 0; i < vars.size; ++i)
    {
        auto treeId = static_cast<libfive::Tree::Id>(vars.vars[i]);
        mapOfVars.insert(std::make_pair(treeId, vars.values[i]));
    }

    return eval_tree->updateVars(mapOfVars);
}

void libfive_evaluator_delete(libfive_evaluator ptr)
{
    // TODO: For more than one worker
    std::cout << "libfive_evaluator_delete";
    delete ptr;
}

////////////////////////////////////////////////////////////////////////////////

// These variables are autogenerated and compiled into the library
// by the version.cmake script
extern "C"
{
    extern const char* GIT_TAG;
    extern const char* GIT_REV;
    extern const char* GIT_BRANCH;
}

const char* libfive_git_version(void)
{
    return GIT_TAG;
}

const char* libfive_git_revision(void)
{
    return GIT_REV;
}

const char* libfive_git_branch(void)
{
    return GIT_BRANCH;
}
