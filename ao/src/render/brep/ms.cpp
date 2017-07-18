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
SquareMarcher::Segment SquareMarcher::cases[16][2] = {
    {{NONE, NONE}, {NONE, NONE}},    // 0000
    {{LOWER, LEFT}, {NONE, NONE}},   // 000a
    {{RIGHT, LOWER}, {NONE, NONE}},  // 00b0
    {{RIGHT, LEFT}, {NONE, NONE}},   // 00ba
    {{LEFT, UPPER}, {NONE, NONE}},   // 0c00
    {{LOWER, UPPER}, {NONE, NONE}},  // 0c0a
    {{RIGHT, LOWER}, {LEFT, UPPER}}, // 0cb0
    {{LEFT, UPPER}, {NONE, NONE}},   // 0cba
    {{UPPER, RIGHT}, {NONE, NONE}},  // d000
    {{LOWER, RIGHT}, {UPPER, LEFT}}, // d00a
    {{UPPER, LOWER}, {NONE, NONE}},  // d0b0
    {{UPPER, LEFT}, {NONE, NONE}},   // d0ba
    {{LEFT, RIGHT}, {NONE, NONE}},   // dc00
    {{LOWER, RIGHT}, {NONE, NONE}},  // dc0a
    {{LEFT, LOWER}, {NONE, NONE}},   // dcb0
    {{NONE, NONE}, {NONE, NONE}},    // dcba
};

std::pair<uint8_t, uint8_t> SquareMarcher::edges[4] = {
    {0, 2}, // LEFT
    {1, 3}, // RIGHT
    {2, 3}, // UPPER
    {0, 1}, // LOWER
};

void SquareMarcher::operator()(const std::array<XTree<2>*, 4>& ts)
{
    for (unsigned i=0; i < 4; ++i)
    {
        eval->set(ts[i]->vert3(), i);
    }
    auto vs = eval->values(4);

    // Build up the bitmask for marching squares
    uint8_t mask = 0;
    for (unsigned i=0; i < 4; ++i)
    {
        mask |= (vs[i] < 0) << i;
    }

    // First segment
    for (unsigned seg=0; seg < 2 && cases[mask][seg].first != NONE; ++seg)
    {
        // Solve for the contour's starting position
        Key a(ts[edges[cases[mask][seg].first].first],
              ts[edges[cases[mask][seg].first].second]);
        if (points.find(a) == points.end())
        {
            points.insert({a, interp.between(a.first->vert, a.second->vert)});
        }

        // Solve for the contour's ending position
        Key b(ts[edges[cases[mask][seg].second].first],
               ts[edges[cases[mask][seg].second].second]);
        if (points.find(b) == points.end())
        {
            points.insert({b, interp.between(b.first->vert, b.second->vert)});
        }

        segments.push_back({a, b});
    }
}

}   // namespace Kernel
