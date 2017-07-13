#include "ao/render/brep/ms.hpp"

namespace Kernel {

/*
 *      Edges are recorded on the following square
 *      c ----- d
 *      |       |   ^ Y
 *      |       |   |
 *      a-------b   ---> X
 */
SquareMarcher::Edge SquareMarcher::cases[16][2][2] = {
    {{NONE, NONE}, {NONE, NONE}},    // 0000
    {{UPPER, RIGHT}, {NONE, NONE}},  // 000d
    {{LEFT, UPPER}, {NONE, NONE}},   // 00c0
    {{LEFT, RIGHT}, {NONE, NONE}},   // 00cd
    {{RIGHT, LOWER}, {NONE, NONE}},  // 0b00
    {{UPPER, LOWER}, {NONE, NONE}},  // 0b0d
    {{RIGHT, LOWER}, {LEFT, UPPER}}, // 0bc0
    {{LEFT, LOWER}, {NONE, NONE}},   // 0bcd

    {{LOWER, LEFT}, {NONE, NONE}},   // a000
    {{LOWER, RIGHT}, {UPPER, LEFT}}, // a00d
    {{LOWER, UPPER}, {NONE, NONE}},  // a0c0
    {{LOWER, RIGHT}, {NONE, NONE}},  // a0cd
    {{RIGHT, LEFT}, {NONE, NONE}},   // ab00
    {{UPPER, LEFT}, {NONE, NONE}},   // ab0d
    {{LEFT, UPPER}, {NONE, NONE}},   // abc0
    {{NONE, NONE}, {NONE, NONE}},    // abcd
};

uint8_t SquareMarcher::edges[4][2] = {
    {0, 2}, // LEFT
    {1, 3}, // RIGHT
    {3, 4}, // UPPER
    {0, 1}, // LOWER
};

void SquareMarcher::operator()(const std::array<XTree<2>*, 4>& ts)
{
    for (unsigned i=0; i < 4; ++i)
    {
        eval->set(ts[i]->vert3(), i);
        std::cout << "Checking vertex [" << ts[i]->vert3().transpose() << "]\n";
    }
    auto vs = eval->values(4);

    // Build up the bitmask for marching squares
    uint8_t mask = 0;
    for (unsigned i=0; i < 4; ++i)
    {
        mask |= (vs[i] < 0) << i;
    }
    std::cout << "Got mask " << int(mask) << '\n';

    // First segment
    for (unsigned seg=0; seg < 2; ++seg)
    {
        if (cases[mask][seg][0] != NONE)
        {
            Key _a(ts[edges[cases[mask][seg][0]][0]],
                   ts[edges[cases[mask][seg][0]][1]]);
            auto a = points.find(_a);
            if (a == points.end())
            {
                auto pt = interp.between(_a.first->vert, _a.second->vert);
                a = points.insert({_a, pt}).first;
            }

            Key _b(ts[edges[cases[mask][seg][1]][0]],
                    ts[edges[cases[mask][seg][1]][1]]);
            auto b = points.find(_b);
            if (b == points.end())
            {
                auto pt = interp.between(_b.first->vert, _b.second->vert);
                b = points.insert({_b, pt}).first;
            }

            segments.push_back({_a, _b});
        }
    }
    std::cout << "Done\n\n";
}

}   // namespace Kernel
