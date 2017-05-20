#include <future>
#include <numeric>
#include <cmath>

#include <glm/geometric.hpp>
#include <glm/gtc/random.hpp>
#include <Eigen/Dense>

#include "kernel/render/xtree.hpp"

namespace Kernel {

template <class T, int dims>
T* XTree<T, dims>::render(const Tree t, const Region& r, bool multithread)
{
    auto rp = r.powerOfTwo(dims).view();

    if (multithread && rp.canSplitEven<dims>())
    {
        std::list<std::future<T*>> futures;

        // Start up a set of future rendering every branch of the octree
        for (auto region : rp.splitEven<dims>())
        {
            auto e = new Evaluator(t);

            futures.push_back(std::async(std::launch::async,
                [e, region](){
                    auto out = new T(e, region);
                    delete e;
                    return out;}));
        }

        // Wait for all of the tasks to finish running in the background
        std::array<T*, 1 << dims> sub;
        int index = 0;
        for (auto& f : futures)
        {
            f.wait();
            sub[index++] = f.get();
        }

        Evaluator e(t);
        return new T(&e, sub, rp);
    }

    else
    {
        Evaluator e(t);
        return new T(&e, rp);
    }
}

////////////////////////////////////////////////////////////////////////////////

template <class T, int dims>
XTree<T, dims>::XTree(const Subregion& r)
    : X(r.X.lower(), r.X.upper()),
      Y(r.Y.lower(), r.Y.upper()),
      Z(r.Z.lower(), r.Z.upper())
{
    // Nothing to do here (delegating constructor)
}

template <class T, int dims>
XTree<T, dims>::XTree(Evaluator* e, const Subregion& r)
    : XTree(r)
{
    populateChildren(e, r);
}

template <class T, int dims>
XTree<T, dims>::XTree(const std::array<T*, 1 << dims>& cs, const Subregion& r)
    : XTree(r)
{
    for (uint8_t i=0; i < cs.size(); ++i)
    {
        children[i].reset(cs[i]);
        corners[i] = children[i]->corners[i];
    }
    type = BRANCH;
}

template <class T, int dims>
void XTree<T, dims>::populateChildren(Evaluator* e, const Subregion& r)
{
    // First, do interval evaluation to see if the cell should be checked
    Interval out = e->eval(r.X.bounds, r.Y.bounds, r.Z.bounds);
    if (out.upper() < 0)
    {
        type = FULL;
    }
    else if (out.lower() > 0)
    {
        type = EMPTY;
    }
    // If the cell wasn't empty or filled, build a BRANCH or LEAF
    else
    {
        bool all_empty = true;
        bool all_full  = true;

        if (r.canSplit())
        {
            auto rs = r.splitEven<dims>();
            e->push();
            for (uint8_t i=0; i < children.size(); ++i)
            {
                // Populate child recursively
                children[i].reset(new T(e, rs[i]));

                // Grab corner values from children
                corners[i] = children[i]->corners[i];

                all_empty &= children[i]->type == EMPTY;
                all_full  &= children[i]->type == FULL;
            }
            e->pop();

            type = all_empty ? EMPTY
                 : all_full ? FULL : BRANCH;
        }
        // Otherwise, calculate corner values
        else
        {
            // Pack into evaluator
            for (uint8_t i=0; i < children.size(); ++i)
            {
                auto c = pos(i);
                e->set(c.x, c.y, c.z, i);
            }

            // Do the evaluation
            const float* fs = e->values(children.size());

            // And unpack from evaluator
            for (uint8_t i=0; i < children.size(); ++i)
            {
                if (fs[i] == 0)
                {
                    const auto c = pos(i);
                    corners[i] = e->isInside(c.x, c.y, c.z);
                }
                else
                {
                    corners[i] = fs[i] < 0;
                }
                all_full  &=  corners[i];
                all_empty &= !corners[i];
            }
            type = all_empty ? EMPTY
                 : all_full ? FULL : LEAF;
        }
    }

    if (type == FULL || type == EMPTY)
    {
        for (auto& c : corners)
        {
            c = type == FULL;
        }
        manifold = true;
    }
}

template <class T, int dims>
void XTree<T, dims>::finalize(Evaluator* e)
{
    // Find this Octree's level
    level = (type == BRANCH)
        ?  std::accumulate(children.begin(), children.end(), (unsigned)0,
                [](const unsigned& a, const std::unique_ptr<T>& b)
                    { return std::max(a, b->level);} ) + 1
        : 0;

    if (type == BRANCH)
    {
        // This populate the vert member if the branch is collapsed
        // into a LEAF node.
        collapseBranch();
    }
    // Always try to convert a LEAF to an EMPTY / FILLED node
    // Otherwise, populate the leaf vertex data
    else if (type == LEAF)
    {
        // Populate matrices, rank, and mass point
        auto solver = findLeafMatrices(e);

        // Figure out if the leaf is manifold
        manifold = this->cornerTopology();

        // Find the vertex for this node
        vert = manifold ? findVertex(solver) :
            // For non-manifold leaf nodes, put the vertex at the mass point.
            // As described in "Dual Contouring: The Secret Sauce", this improves
            // mesh quality.
            glm::vec3(mass_point.x, mass_point.y, mass_point.z) / mass_point.w;
    }

    // If this cell is no longer a branch, remove its children
    if (type != BRANCH)
    {
        std::for_each(children.begin(), children.end(),
            [](std::unique_ptr<T>& o) { o.reset(); });
    }
}

template <class T, int dims>
std::vector<Intersection> XTree<T, dims>::findIntersections(
        Evaluator* eval) const
{
    assert(type == LEAF);

    // Check every edge and use binary search to find intersections on
    // edges that have mismatched signs
    std::vector<glm::vec3> pts;
    for (auto e : static_cast<const T*>(this)->cellEdges())
    {
        if (corner(e.first) != corner(e.second))
        {
            const glm::vec3 p = corner(e.first)
                ? searchEdge(pos(e.first), pos(e.second), eval)
                : searchEdge(pos(e.second), pos(e.first), eval);

            // Store position in big list o' intersections
            // (along with a dummy normal)
            pts.push_back(p);
        }
    }

    // Calculate normals in bulk (since that's more efficient)
    for (unsigned i=0; i < pts.size(); ++i)
    {
        eval->setRaw(pts[i].x, pts[i].y, pts[i].z, i);
    }
    auto ds = eval->derivs(pts.size());

    // Mark which of the points are ambiguous
    const auto ambiguous = eval->getAmbiguous(pts.size());

    // Accumulate intersections, ambiguous and non-ambiguous
    std::vector<Intersection> intersections;
    for (unsigned i=0; i < pts.size(); ++i)
    {
        const auto p = pts[i];
        if (ambiguous.find(i) == ambiguous.end())
        {
            const glm::vec3 g(ds.dx[i], ds.dy[i], ds.dz[i]);
            intersections.push_back({p, glm::normalize(g)});
        }
        else
        {
            for (auto& f : eval->featuresAt(p.x, p.y, p.z))
            {
                if (f.isCompatible(f.deriv) && f.isCompatible(-f.deriv))
                {
                    intersections.push_back({p, f.deriv});
                }
            }
        }
    }

    return intersections;
}


template <class T, int dims>
void XTree<T, dims>::findBranchMatrices()
{
    // Find the max rank among children, then only accumulate
    // intersections from children with that rank
    rank = std::accumulate(
            children.begin(), children.end(), (unsigned)0,
            [](const unsigned& a, const std::unique_ptr<T>& b)
                { return std::max(a, b->rank);} );

    for (const auto& c : children)
    {
        if (c->rank == rank)
        {
            mass_point += c->mass_point;
        }
        AtA += c->AtA;
        AtB += c->AtB;
        BtB += c->BtB;
    }
}

template <class T, int dims>
void XTree<T, dims>::collapseBranch()
{
    if (std::all_of(children.begin(), children.end(),
                    [](const std::unique_ptr<T>& o)
                    { return o->type != BRANCH; }))
    {
        //  This conditional implements the three checks described in
        //  [Ju et al, 2002] in the section titled
        //      "Simplification with topology safety"
        manifold = this->cornerTopology() &&
            std::all_of(children.begin(), children.end(),
                    [](const std::unique_ptr<T>& o)
                    { return o->manifold; }) &&
            static_cast<T*>(this)->leafTopology();

        if (manifold)
        {
            findBranchMatrices();

            float err;
            vert = findVertex(&err);
            if (err < 1e-8)
            {
                type = LEAF;
            }
        }
    }
}

template <class T, int dims>
Eigen::EigenSolver<Eigen::Matrix3d> XTree<T, dims>::findLeafMatrices(Evaluator* e)
{
    std::vector<Intersection> intersections = findIntersections(e);

    /*  The A matrix is of the form
     *  [n1x, n1y, n1z]
     *  [n2x, n2y, n2z]
     *  [n3x, n3y, n3z]
     *  ...
     *  (with one row for each Hermite intersection)
     */
    Eigen::MatrixX3d A(intersections.size(), 3);

    /*  The B matrix is of the form
     *  [p1 . n1]
     *  [p2 . n2]
     *  [p3 . n3]
     *  ...
     *  (with one row for each Hermite intersection)
     */
    Eigen::VectorXd B(intersections.size(), 1);
    for (unsigned i=0; i < intersections.size(); ++i)
    {
        const auto norm = intersections[i].norm;
        const auto pos  = intersections[i].pos;

        // Build up matrices
        A.row(i) << Eigen::Vector3d(norm.x, norm.y, norm.z).transpose();
        B.row(i) << glm::dot(norm, pos);

        // Accumulate intersection in mass point
        mass_point += glm::vec4(pos, 1.0f);
    }

    auto At = A.transpose();

    AtA = At * A;
    AtB = At * B;
    BtB = B.transpose() * B;

    // Use eigenvalues to find rank, then return the solver
    // (so it can be re-used to find vertex position)
    Eigen::EigenSolver<Eigen::Matrix3d> es(AtA);
    auto eigenvalues = es.eigenvalues().real();

    // Truncate near-singular eigenvalues
    rank = (eigenvalues.array().abs() >= EIGENVALUE_CUTOFF).count();

    // Return the solver so it can be re-used
    return es;
}

template <class T, int dims>
glm::vec3 XTree<T, dims>::findVertex(float* err) const
{
    Eigen::EigenSolver<Eigen::Matrix3d> es(AtA);
    return findVertex(es, err);
}

template <class T, int dims>
glm::vec3 XTree<T, dims>::findVertex(
        Eigen::EigenSolver<Eigen::Matrix3d>& es,
        float* err) const
{
    // We need to find the pseudo-inverse of AtA.
    auto eigenvalues = es.eigenvalues().real();

    // Truncate near-singular eigenvalues in the SVD's diagonal matrix
    Eigen::Matrix3d D(Eigen::Matrix3d::Zero());
    for (unsigned i=0; i < 3; ++i)
    {
        D.diagonal()[i] = (std::abs(eigenvalues[i]) < EIGENVALUE_CUTOFF)
            ? 0 : (1 / eigenvalues[i]);
    }

    // Sanity-checking that rank matches eigenvalue count
    if (type == LEAF)
    {
        assert(D.diagonal().count() == rank);
    }

    // SVD matrices
    auto U = es.eigenvectors().real(); // = V

    // Pseudo-inverse of A
    auto AtAp = U * D * U.transpose();

    // Solve for vertex (minimizing distance to center)
    auto p = Eigen::Vector3d(mass_point.x, mass_point.y, mass_point.z) /
             mass_point.w;
    auto v = AtAp * (AtB - AtA * p) + p;

    // Find the QEF error if required
    if (err)
    {
        *err = (v.transpose() * AtA * v - 2*v.transpose() * AtB)[0] + BtB;
    }

    // Convert out of Eigen's format and return
    return glm::vec3(v[0], v[1], v[2]);
}


template <class T, int dims>
bool XTree<T, dims>::cornerTopology() const
{
    uint8_t index = 0;
    for (uint8_t i=0; i < children.size(); ++i)
    {
        if (corners[i])
        {
            index |= (1 << i);
        }
    }

    return static_cast<const T*>(this)->cornerTable()[index];
}

template<class T, int dims>
glm::vec3 XTree<T, dims>::searchEdge(glm::vec3 a, glm::vec3 b,
                                     Evaluator* e) const
{
    // We do an N-fold reduction at each stage
    constexpr int _N = 4;
    constexpr int N = (1 << _N);
    constexpr int ITER = SEARCH_COUNT / _N;

    // Binary search for intersection
    for (int i=0; i < ITER; ++i)
    {
        glm::vec3 ps[N];
        for (int j=0; j < N; ++j)
        {
            float frac = j / (N - 1.0);
            ps[j] = (a * (1 - frac)) + (b * frac);
            e->setRaw(ps[j].x, ps[j].y, ps[j].z, j);
        }

        auto out = e->values(N);
        for (int j=0; j < N; ++j)
        {
            if (out[j] > 0 ||
                (out[j] == 0 && !e->isInside(ps[j].x, ps[j].y, ps[j].z)))
            {
                a = ps[j - 1];
                b = ps[j];
                break;
            }
        }
    }

    return a;
}

}   // namespace Kernel
