#pragma once

#include "ao/render/xtree.hpp"

namespace Kernel {

class Octree : public XTree<Octree, 3>
{
protected:
    Octree(const Subregion& r);
    Octree(Evaluator* e, const Subregion& r);
    Octree(Evaluator* e, const std::array<Octree*, 8>& cs, const Subregion& r);

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
