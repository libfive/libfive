#pragma once

#include "ao/tree/tree.hpp"

#include "ao/render/axes.hpp"
#include "ao/render/brep/region.hpp"
#include "ao/render/brep/brep.hpp"
#include "ao/render/brep/xtree.hpp"

namespace Kernel {

class Mesh : public BRep<3> {
public:
    /*
     *  Blocking, unstoppable render function
     */
    static std::unique_ptr<Mesh> render(const Tree t, const Region<3>& r,
                                        double min_feature=0.1, double max_err=1e-8);

    /*
     *  Fully-specified render function
     */
    static std::unique_ptr<Mesh> render(
            const Tree t, const std::map<Tree::Id, float>& vars,
            const Region<3>& r, double min_feature, double max_err,
            std::atomic_bool& cancel);

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
    void line(Eigen::Vector3f a, Eigen::Vector3f b);
};

}   // namespace Kernel
