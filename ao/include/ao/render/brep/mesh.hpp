#pragma once

#include "ao/tree/tree.hpp"

#include "ao/render/brep/region.hpp"
#include "ao/render/brep/brep.hpp"
#include "ao/render/brep/xtree.hpp"

namespace Kernel {

class Mesh : public BRep<3> {
public:
    static std::unique_ptr<Mesh> render(const Tree t, const Region<3>& r);

    /*
     *  Writes the mesh to a file
     */
    bool saveSTL(const std::string& filename);

    /*
     *  Called by Dual::walk to construct the triangle mesh
     */
    void operator()(const std::array<const XTree<3>*, 4>& ts);
};

}   // namespace Kernel
