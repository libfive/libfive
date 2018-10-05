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
#include "libfive/render/brep/progress.hpp"

namespace Kernel {

class Mesh : public BRep<3> {
public:
    /*
     *  Blocking, unstoppable render function
     *  Returns nullptr if min_feature is invalid (i.e. <= 0)
     */
    static std::unique_ptr<Mesh> render(
            const Tree t, const Region<3>& r,
            double min_feature=0.1, double max_err=1e-8, bool multithread=true,
            ProgressCallback progress_callback=EMPTY_PROGRESS_CALLBACK);

    /*
     *  Fully-specified render function
     *  Returns nullptr if min_feature is invalid or cancel is set to true
     *  partway through the computation.
     */
    static std::unique_ptr<Mesh> render(
            const Tree t, const std::map<Tree::Id, float>& vars,
            const Region<3>& r, double min_feature, double max_err,
            unsigned workers, std::atomic_bool& cancel,
            ProgressCallback progress_callback=EMPTY_PROGRESS_CALLBACK);

    /*
     *  Render function that re-uses evaluators
     *  es must be a pointer to at least workers Evaluators
     *  Returns nullptr if min_feature is invalid or cancel is set to true
     *  partway through the computation.
     */
    static std::unique_ptr<Mesh> render(
            XTreeEvaluator* es, const Region<3>& r,
            double min_feature, double max_err,
            int workers, std::atomic_bool& cancel,
            ProgressCallback progress_callback=EMPTY_PROGRESS_CALLBACK);

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
     *  Called by Dual::walk to construct the triangle mesh.  ts[index]
     *  should be a minimal-level member of ts (though properly, it need
     *  merely contain the edge being loaded as one of its edges).
     */
    template <Axis::Axis A, bool D>
    void load(const std::array<const XTree<3>*, 4>& ts, unsigned index);

    /*  Walks an XTree, returning a mesh  */
    static std::unique_ptr<Mesh> mesh(
            const XTree<3>::Root& tree, std::atomic_bool& cancel,
            ProgressCallback progress_callback=EMPTY_PROGRESS_CALLBACK);

    static const float MAX_PROGRESS;

protected:
    /*
     *  Inserts a line into the mesh as a zero-size triangle
     *  (used for debugging)
     */
    void line(const Eigen::Vector3f& a, const Eigen::Vector3f& b);


    /*
     *  It is possible for a triangle to escape the cells that generated it, 
     *  if said cells are of different sizes.  This function adds branes in
     *  such a way that, so long as the endpoints are in their proper cells,
     *  the entire triangle will be as well.  The axis and direction are not
     *  those of the load function that calls this, but rather from a to b.
     *  a and b must be ordered such that aIndex, bIndex, intersectionIndex 
     *  is the proper winding for this triangle.
     */
    template <Axis::Axis A, bool D>
    void checkAndAddTriangle(const XTree<3>* a, const XTree<3>* b, 
                             uint32_t aIndex, uint32_t bIndex, 
                             uint32_t intersectionIndex);

    /*
     *  Used to store the indices of vertices created by forcing triangles to
     *  be inside the cells generating them.  The key values are the 
     *  corresponding cell vertices in ascending order, and the resulting 
     *  vertex may or may not be an intersection vertex.
     */
    std::map<std::pair<uint32_t, uint32_t>, uint32_t> forcedVerts;

};

}   // namespace Kernel
