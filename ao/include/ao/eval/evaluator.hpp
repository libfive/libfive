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

class Evaluator
{
public:
    /*
     *  Construct an evaluator for the given tree
     */
    Evaluator(const Tree root) : Evaluator(root, std::map<Tree::Id, float>())
        { /* Nothing to do here */ }
    Evaluator(const Tree root, const std::map<Tree::Id, float>& vars);

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*
     *  Single-argument evaluation
     */
    float eval(const Eigen::Vector3f& p);
    Interval::I eval(const Eigen::Vector3f& lower, const Eigen::Vector3f& upper);

    /*
     *  Evaluates the given point using whichever tape in the tape stack
     *  contains the point in its region (this is useful when we're not
     *  sure about which region the points fits into)
     */
    float baseEval(const Eigen::Vector3f& p);

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
        const Eigen::Array<float, 3, Result::N>& d;
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
    Interval::I interval();

    /*
     *  Stores the given value in the result arrays
     *  (inlined for efficiency)
     */
    void set(const Eigen::Vector3f& p, Result::Index index)
    {
        result->f(X, index) = p.x();
        result->f(Y, index) = p.y();
        result->f(Z, index) = p.z();
    }

    /*
     *  Stores the given interval in the result objects
     */
    void set(const Eigen::Vector3f& lower, const Eigen::Vector3f& upper);

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
    const Eigen::Array<bool, Result::N, 1>& getAmbiguous(Result::Index i);

    /*
     *  Checks whether the given position is ambiguous
     */
    bool isAmbiguous(const Eigen::Vector3f& p);

    /*
     *  Checks whether the result in slot 0 is ambiguous
     */
    bool isAmbiguous();

protected:
    /*  This is our evaluation tape type */
    struct Tape {
        std::vector<Clause> t;
        Clause::Id i;
        Interval::I X, Y, Z;
        enum Type { UNKNOWN, INTERVAL, SPECIALIZED, FEATURE } type;
    };

    /*
     *  Pushes a new tape onto the stack, storing it in tape
     *
     *  Requires disabled and remap both to contain useful data; this is used
     *  when deciding which clauses to push into the new tape.
     */
    void pushTape(Tape::Type t);

    /*
     *  Evaluate the tree's values and Jacobian
     *  with respect to all its variables
     */
    static void eval_clause_jacobians(Opcode::Opcode op,
        const float av,  std::vector<float>& aj,
        const float bv,  std::vector<float>& bj,
                         std::vector<float>& oj);

    /*
     *  Evaluates a single Interval clause
     */
    static Interval::I eval_clause_interval(
        Opcode::Opcode op, const Interval::I& a, const Interval::I& b);

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

    std::unique_ptr<Result> result;
};

}   // namespace Kernel
