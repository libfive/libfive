#pragma once

#include "kernel/render/xtree.hpp"

namespace Kernel {

class Quadtree : public XTree<Quadtree, 2>
{
    Quadtree(const Subregion& r);
    Quadtree(Evaluator* e, const Subregion& r);
    Quadtree(Evaluator* e, const std::array<Quadtree*, 4>& cs,
             const Subregion& r);

    static const std::vector<bool>& cornerTable()
        { return _cornerTable; }
    static const std::vector<std::pair<unsigned, unsigned>>& cellEdges()
        { return _cellEdges; }
    bool leafTopology() const;

    const static std::vector<std::pair<unsigned, unsigned>> _cellEdges;
    const static std::vector<bool> _cornerTable;

    friend class XTree;
};

}   // namespace Kernel
