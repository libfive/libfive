#include "ao/render/brep/ms.hpp"

namespace Kernel {

/*
 *      Edges are recorded on the following square
 *      c2-----3d
 *      |       |   ^ Y
 *      |       |   |
 *      a0-----1b   ---> X
 *
 */
#define NONE {-1, -1}
const SquareMarcher::Edge SquareMarcher::cases[16][2][2] = {
    {{NONE,   NONE},    {NONE,   NONE}},    // 0000
    {{{0, 1}, {0, 2}},  {NONE,   NONE}},    // 000a
    {{{1, 3}, {1, 0}},  {NONE,   NONE}},    // 00b0
    {{{1, 3}, {0, 2}},  {NONE,   NONE}},    // 00ba
    {{{2, 0}, {2, 3}},  {NONE,   NONE}},    // 0c00
    {{{0, 1}, {2, 3}},  {NONE,   NONE}},    // 0c0a
    {{{2, 0}, {2, 3}},  {{1, 3}, {1, 0}}},  // 0cb0
    {{{1, 3}, {2, 3}},  {NONE,   NONE}},    // 0cba

    {{{3, 2}, {3, 1}},  {NONE,   NONE}},    // d000
    {{{3, 2}, {3, 1}},  {{0, 1}, {0, 2}}},  // d00a
    {{{3, 2}, {1, 0}},  {NONE,   NONE}},    // d0b0
    {{{3, 2}, {0, 2}},  {NONE,   NONE}},    // d0ba
    {{{2, 0}, {3, 1}},  {NONE,   NONE}},    // dc00
    {{{0, 1}, {3, 1}},  {NONE,   NONE}},    // dc0a
    {{{2, 0}, {1, 0}},  {NONE,   NONE}},    // dcb0
    {{NONE,   NONE},    {NONE,   NONE}},    // dcba
};

void SquareMarcher::operator()(const std::array<XTree<2>*, 4>& ts)
{
    for (unsigned i=0; i < ts.size(); ++i)
    {
        eval->set(ts[i]->vert3(), i);
    }
    auto vs = eval->values(ts.size());

    // Build up the bitmask for marching squares
    uint8_t mask = 0;
    for (unsigned i=0; i < ts.size(); ++i)
    {
        mask |= (vs[i] < 0) << i;
    }

    // Iterate over up to two segments in the square, aborting early if this
    // particular configuration has one or zero segments.
    for (unsigned seg=0; seg < 2 && cases[mask][seg][0].first != -1; ++seg)
    {
        uint32_t s[2];
        for (unsigned v=0; v < 2; ++v)
        {
            // Construct a dual edge key
            Key _k(ts[cases[mask][seg][v].first],
                   ts[cases[mask][seg][v].second]);

            auto k = indices.find(_k);
            if (k == indices.end())
            {
                // Make sure we're interpolating between valid vertices
                // (which have had their QEFs solved)
                assert(_k.first->err != -1);
                assert(_k.second->err != -1);

                auto pt = interp.between(_k.first->vert, _k.second->vert);
                k = indices.insert({_k, pts.size()}).first;
                pts.push_back(pt);
            }
            s[v] = k->second;
        }
        segments.push_back({s[0], s[1]});
    }
}

}   // namespace Kernel
