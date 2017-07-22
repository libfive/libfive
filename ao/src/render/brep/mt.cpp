#include "ao/render/brep/mt.hpp"

namespace Kernel {

static const std::array<uint8_t, 6> VERTEX_LOOP = {{6, 4, 5, 1, 3, 2}};

// Based on which vertices are filled, this map tells you which
// edges to interpolate between when forming zero, one, or two
// triangles (filled vertex is first in the pair)
static const std::pair<int8_t, int8_t> EDGE_MAP[16][2][3] = {
    {{{-1,-1}, {-1,-1}, {-1,-1}}, {{-1,-1}, {-1,-1}, {-1,-1}}}, // ----
    {{{ 0, 2}, { 0, 1}, { 0, 3}}, {{-1,-1}, {-1,-1}, {-1,-1}}}, // ---0
    {{{ 1, 0}, { 1, 2}, { 1, 3}}, {{-1,-1}, {-1,-1}, {-1,-1}}}, // --1-
    {{{ 1, 2}, { 1, 3}, { 0, 3}}, {{ 0, 3}, { 0, 2}, { 1, 2}}}, // --10
    {{{ 2, 0}, { 2, 3}, { 2, 1}}, {{-1,-1}, {-1,-1}, {-1,-1}}}, // -2--
    {{{ 0, 3}, { 2, 3}, { 2, 1}}, {{ 2, 1}, { 0, 1}, { 0, 3}}}, // -2-0
    {{{ 1, 0}, { 2, 0}, { 2, 3}}, {{ 2, 3}, { 1, 3}, { 1, 0}}}, // -21-
    {{{ 2, 3}, { 1, 3}, { 0, 3}}, {{-1,-1}, {-1,-1}, {-1,-1}}}, // -210

    {{{ 3, 0}, { 3, 1}, { 3, 2}}, {{-1,-1}, {-1,-1}, {-1,-1}}}, // 3---
    {{{ 3, 2}, { 0, 2}, { 0, 1}}, {{ 0, 1}, { 3, 1}, { 3, 2}}}, // 3--0
    {{{ 1, 2}, { 3, 2}, { 3, 0}}, {{ 3, 0}, { 1, 0}, { 1, 2}}}, // 3-1-
    {{{ 1, 2}, { 3, 2}, { 0, 2}}, {{-1,-1}, {-1,-1}, {-1,-1}}}, // 3-10
    {{{ 3, 0}, { 3, 1}, { 2, 1}}, {{ 2, 1}, { 2, 0}, { 3, 0}}}, // 32--
    {{{ 3, 1}, { 2, 1}, { 0, 1}}, {{-1,-1}, {-1,-1}, {-1,-1}}}, // 32-0
    {{{ 3, 0}, { 1, 0}, { 2, 0}}, {{-1,-1}, {-1,-1}, {-1,-1}}}, // 321-
    {{{-1,-1}, {-1,-1}, {-1,-1}}, {{-1,-1}, {-1,-1}, {-1,-1}}}, // 3210
};

void TetMarcher::operator()(const std::array<XTree<3>*, 8>& ts)
{
    // Loop over the six tetrahedra that make up a voxel cell
    for (unsigned t = 0; t < VERTEX_LOOP.size(); ++t)
    {
        // Find vertex indices (into vs or ts) for this tetrahedron
        const std::array<uint8_t, 4> vertices =
            {{0, 7, VERTEX_LOOP[t], VERTEX_LOOP[(t + 1) % VERTEX_LOOP.size()]}};

        // Build up the bitmask for this tetrahedron
        uint8_t mask = 0;
        for (unsigned i=0; i < vertices.size(); ++i)
        {
            mask |= (ts[vertices[i]]->value < 0) << i;
        }

        // Iterate over up to two triangles in the tet, aborting early if
        // this particular configuration has one or zero triangles.
        for (unsigned t=0; t < 2 && EDGE_MAP[mask][t][0].first != -1; ++t)
        {
            uint32_t tri[3];
            for (int v=0; v < 3; ++v)
            {
                //  Construct a dual edge key
                Key _k(ts[vertices[EDGE_MAP[mask][t][v].first]],
                       ts[vertices[EDGE_MAP[mask][t][v].second]]);

                // Make sure we're interpolating between valid vertices
                // (which have had their QEFs solved)
                assert(_k.first->err != -1);
                assert(_k.second->err != -1);

                // Check if we have already searched along this dual edge
                auto k = indices.find(_k);
                if (k == indices.end())
                {
                    auto pt = interp.between(_k.first->vert, _k.second->vert);
                    k = indices.insert({_k, verts.size()}).first;
                    verts.push_back(pt);
                }
                tri[v] = k->second;
            }
            branes.push_back({{tri[2], tri[1], tri[0]}});
        }
    }
}

}   // namespace Kernel
