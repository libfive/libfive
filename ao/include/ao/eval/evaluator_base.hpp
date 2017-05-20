#pragma once

#include <array>
#include <unordered_map>
#include <vector>
#include <map>

#include <glm/mat4x4.hpp>
#include <boost/bimap.hpp>

#include "kernel/eval/result.hpp"
#include "kernel/eval/interval.hpp"
#include "kernel/eval/feature.hpp"
#include "kernel/eval/clause.hpp"
#include "kernel/tree/tree.hpp"

////////////////////////////////////////////////////////////////////////////////

namespace Kernel {

class EvaluatorBase
{
public:
    /*
     *  Construct an evaluator for the given tree
     */
    EvaluatorBase(const Tree root, const glm::mat4& M=glm::mat4(),
                  const std::map<Tree::Id, float>& vars=
                        std::map<Tree::Id, float>());
    EvaluatorBase(const Tree root, const std::map<Tree::Id, float>& vars)
        : EvaluatorBase(root, glm::mat4(), vars) {}

    /*
     *  Single-argument evaluation
     */
    float eval(float x, float y, float z);
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
    std::map<Tree::Id, float> gradient(float x, float y, float z);

    /*
     *  Evaluates a single interval (stored with set)
     */
    Interval interval();

    /*
     *  Stores the given value in the result arrays
     *  (inlined for efficiency)
     */
    void set(float x, float y, float z, Result::Index index)
    {
        result.f[X][index] = M[0][0] * x + M[1][0] * y + M[2][0] * z + M[3][0];
        result.f[Y][index] = M[0][1] * x + M[1][1] * y + M[2][1] * z + M[3][1];
        result.f[Z][index] = M[0][2] * x + M[1][2] * y + M[2][2] * z + M[3][2];
    }

    /*
     *  Unsafe setter (which requires a call to applyTransform afterwards)
     */
    void setRaw(float x, float y, float z, Result::Index index)
    {
        result.f[X][index] = x;
        result.f[Y][index] = y;
        result.f[Z][index] = z;
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
    void setMatrix(const glm::mat4& m);

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
    void specialize(float x, float y, float z);

    /*
     *  Checks to see if the given point is inside the solid body.
     *  There are three cases
     *      eval(x, y, z) < 0  => true
     *      eval(x, y, z) > 0  => false
     *      eval(x, y, z) == 0 => further checking is performed
     */
    bool isInside(float x, float y, float z);

    /*
     *  Checks for features at the given position
     */
    std::list<Feature> featuresAt(float x, float y, float z);

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
    glm::mat4 M;
    glm::mat4 Mi;

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
