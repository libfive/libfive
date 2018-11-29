#include "catch.hpp"
#include "mesh_checks.hpp"

void CHECK_EDGE_PAIRS(const Kernel::Mesh& m) {
    // Every edge must be shared by two triangles
    // We build a bitfield here, counting forward and reverse edges
    std::map<std::pair<int, int>, int> edges;
    for (const auto& t : m.branes) {
        for (unsigned i=0; i < 3; ++i) {
            const auto a = t[i];
            const auto b = t[(i + 1) % 3];
            auto key = std::make_pair(std::min(a, b), std::max(a, b));
            if (!edges.count(key)) {
                edges.insert({key, 0});
            }
            if (a < b)
            {
                REQUIRE((edges[key] & 1) == 0);
                edges[key] |= 1;
            }
            else
            {
                REQUIRE((edges[key] & 2) == 0);
                edges[key] |= 2;
            }
        }
    }
    for (auto& p : edges) {
        CAPTURE(p.first.first);
        CAPTURE(p.first.second);
        CAPTURE(m.verts[p.first.first]);
        CAPTURE(m.verts[p.first.second]);
        REQUIRE(p.second == 3);
    }
}

