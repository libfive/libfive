/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#pragma once

#include "libfive/tree/tree.hpp"

#include "libfive/render/axes.hpp"
#include "libfive/render/brep/region.hpp"
#include "libfive/render/brep/brep.hpp"
#include "libfive/render/brep/xtree.hpp"

namespace Kernel {

class Mesh : public BRep<3> {
public:
    /*
     *  Blocking, unstoppable render function
     */
    static std::unique_ptr<Mesh> render(const Tree t, const Region<3>& r,
                                        double min_feature=0.1,
                                        double max_err=1e-8,
                                        bool multithread=true);

    /*
     *  Fully-specified render function
     */
    static std::unique_ptr<Mesh> render(
            const Tree t, const std::map<Tree::Id, float>& vars,
            const Region<3>& r, double min_feature, double max_err,
            bool multithread, std::atomic_bool& cancel);

    /*
     *  Render function that re-uses evaluators
     *  es must be a pointer to at least eight Evaluators
     */
    static std::unique_ptr<Mesh> render(
            XTreeEvaluator* es,
            const Region<3>& r, double min_feature, double max_err,
            std::atomic_bool& cancel);

    /*
     *  Writes the mesh to a file
     */
    bool saveSTL(const std::string& filename);

    /*
     *  Merge multiple bodies and write them to a single file
     */
    static bool saveSTL(const std::string& filename,
                        const std::list<const Mesh*>& meshes);

    /*
     *  Called by Dual::walk to construct the triangle mesh
     */
    template <Axis::Axis A, bool D>
    void load(const std::array<const XTree<3>*, 4>& ts);

protected:
    /*  Walks an XTree, returning a mesh  */
    static std::unique_ptr<Mesh> mesh(std::unique_ptr<const XTree<3>> tree,
                                      std::atomic_bool& cancel);

    /*
     *  Inserts a line into the mesh as a zero-size triangle
     *  (used for debugging)
     */
    void line(const Eigen::Vector3f& a, const Eigen::Vector3f& b);
};

}   // namespace Kernel
