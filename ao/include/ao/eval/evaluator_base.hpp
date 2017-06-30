#pragma once

#include <array>
#include <unordered_map>
#include <vector>
#include <map>

#include <Eigen/Eigen>
#include <boost/bimap.hpp>

#include "ao/eval/result.hpp"
#include "ao/eval/interval.hpp"
#include "ao/eval/feature.hpp"
#include "ao/eval/clause.hpp"
#include "ao/tree/tree.hpp"

////////////////////////////////////////////////////////////////////////////////

namespace Kernel {

class EvaluatorBase
{
public:
    /*
     *  Construct an evaluator for the given tree
     */
    EvaluatorBase(const Tree root,
                  const Eigen::Matrix4f& M=Eigen::Matrix4f::Identity(),
                  const std::map<Tree::Id, float>& vars=
                        std::map<Tree::Id, float>());
    EvaluatorBase(const Tree root, const std::map<Tree::Id, float>& vars)
        : EvaluatorBase(root, Eigen::Matrix4f::Identity(), vars) {}

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*
     *  Single-argument evaluation
     */
    float eval(const Eigen::Vector3f& p);
    Interval eval(Interval x, Interval y, Interval z);

    /*
     *  Evaluates a set of floating-point results
     *  (which have been loaded with set)
     */
    const float* values(Result::Index count);

    /*
     *  Helper struct when returning derivatives
     */
    struct Derivs {
        const float* v;
        const float* dx;
        const float* dy;
        const float* dz;
    };

    /*
     *  Evaluate a set of gradients, returning a tuple
     *      value, dx, dy, dz
     *
     *  Values must have been previously loaded by set
     */
    Derivs derivs(Result::Index count);

    /*
     *  Returns the gradient with respect to all VAR nodes
     */
    std::map<Tree::Id, float> gradient(const Eigen::Vector3f& p);

    /*
     *  Evaluates a single interval (stored with set)
     */
    Interval interval();

    /*
     *  Stores the given value in the result arrays
     *  (inlined for efficiency)
     */
    void set(const Eigen::Vector3f& p, Result::Index index)
    {
        result.f[X][index] = M(0,0) * p.x() + M(0,1) * p.y() + M(0,2) * p.z() + M(0,3);
        result.f[Y][index] = M(1,0) * p.x() + M(1,1) * p.y() + M(1,2) * p.z() + M(1,3);
        result.f[Z][index] = M(2,0) * p.x() + M(2,1) * p.y() + M(2,2) * p.z() + M(2,3);
    }

    /*
     *  Unsafe setter (which requires a call to applyTransform afterwards)
     */
    void setRaw(const Eigen::Vector3f& p, Result::Index index)
    {
        result.f[X][index] = p.x();
        result.f[Y][index] = p.y();
        result.f[Z][index] = p.z();
    }

    /*
     *  Applies M to values stored by set
     */
    void applyTransform(Result::Index count);

    /*
     *  Stores the given interval in the result objects
     */
    void set(Interval X, Interval Y, Interval Z);

    /*
     *  Pushes into a subinterval, disabling inactive nodes
     */
    void push();

    /*
     *  Pushes into a tree based on the given feature
     *
     *  result.f[][0] must contain evaluation results with a matching
     *  number of ambiguous min/max nodes as the given feature (or more,
     *  if the feature is partial)
     *
     *  Returns the feature f with non-relevant choices removed
     */
    Feature push(const Feature& f);

    /*
     *  Pops out of interval evaluation, re-enabling disabled nodes
     */
    void pop();

    /*
     *  Returns the fraction active / total nodes
     *  (to check how well disabling is working)
     */
    double utilization() const;

    /*
     *  Sets the global matrix transform
     *  Invalidates all positions and results
     */
    void setMatrix(const Eigen::Matrix4f& m);

    /*
     *  Changes a variable's value
     *
     *  If the variable isn't present in the tree, does nothing
     */
    void setVar(Tree::Id var, float value);

    /*
     *  Returns the current values associated with all variables
     */
    std::map<Tree::Id, float> varValues() const;

    /*
     *  Updates variable values, return true if changed
     */
    bool updateVars(const std::map<Kernel::Tree::Id, float>& vars);

    /*
     *  Pushes into a tree with min/max nodes specialized
     *  based on evaluation at the given point
     */
    void specialize(const Eigen::Vector3f& p);

    /*
     *  Checks to see if the given point is inside the solid body.
     *  There are three cases
     *      eval(x, y, z) < 0  => true
     *      eval(x, y, z) > 0  => false
     *      eval(x, y, z) == 0 => further checking is performed
     */
    bool isInside(const Eigen::Vector3f& p);

    /*
     *  Checks for features at the given position
     */
    std::list<Feature> featuresAt(const Eigen::Vector3f& p);

    /*
     *  Returns a list of ambiguous items from indices 0 to i
     */
    std::set<Result::Index> getAmbiguous(Result::Index i) const;

protected:
    /*  This is our evaluation tape type */
    struct Tape {
        std::vector<Clause> t;
        Clause::Id i;
    };

    /*
     *  Pushes a new tape onto the stack, storing it in tape
     *
     *  Requires disabled and remap both to contain useful data; this is used
     *  when deciding which clauses to push into the new tape.
     */
    void pushTape();

    /*
     *  Evaluate a single clause, populating the out array
     */
    static void eval_clause_values(Opcode::Opcode op,
        const float* __restrict a, const float* __restrict b,
        float* __restrict out, Result::Index count);

    /*
     *  Evaluate a set of derivatives (X, Y, Z)
     */
    static void eval_clause_derivs(Opcode::Opcode op,
        const float* __restrict av,  const float* __restrict adx,
        const float* __restrict ady, const float* __restrict adz,

        const float* __restrict bv,  const float* __restrict bdx,
        const float* __restrict bdy, const float* __restrict bdz,

        float* __restrict ov,  float* __restrict odx,
        float* __restrict ody, float* __restrict odz,
        Result::Index count);

    /*
     *  Evaluate the tree's values and Jacobian
     *  with respect to all its variables
     */
    static float eval_clause_jacobians(Opcode::Opcode op,
        const float av,  std::vector<float>& aj,
        const float bv,  std::vector<float>& bj,
                         std::vector<float>& oj);

    /*
     *  Evaluates a single Interval clause
     */
    static Interval eval_clause_interval(
        Opcode::Opcode op, const Interval& a, const Interval& b);

    /*  Global matrix transform (and inverse) applied to all coordinates  */
    Eigen::Matrix4f M;
    Eigen::Matrix4f Mi;

    /*  Indices of X, Y, Z coordinates */
    Clause::Id X, Y, Z;

    /*  Map of variables (in terms of where they live in this Evaluator) to
     *  their ids in their respective Tree (e.g. what you get when calling
     *  Tree::var().id() */
    boost::bimap<Clause::Id, Tree::Id> vars;
    /*  We also store shared-pointer handles to var Trees, so we can
     *  reconstruct a proper Tree from the Evaluator  */
    std::map<Tree::Id, Tree> var_handles;

    /*  Tape containing our opcodes in reverse order */
    std::list<Tape> tapes;
    std::list<Tape>::iterator tape;

    /*  Store the root opcode explicitly so that we can convert back into
     *  a tree even if there's nothing in the tape  */
    const Opcode::Opcode root_op;

    std::vector<uint8_t> disabled;
    std::vector<Clause::Id> remap;

    Result result;
};

}   // namespace Kernel
