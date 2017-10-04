#pragma once

#include <vector>

#include <boost/bimap.hpp>
#include <Eigen/Eigen>

#include "ao/eval/clause.hpp"
#include "ao/eval/interval.hpp"
#include "ao/tree/tree.hpp"

#include "ao/render/brep/region.hpp"

namespace Kernel {

class Tape
{
public:
    Tape(const Tree root);

    /*  Returned by evaluator types when pushing  */
    enum Keep { KEEP_BOTH, KEEP_A, KEEP_B };

    /*  Different kind of tape pushes  */
    enum Type { UNKNOWN, INTERVAL, SPECIALIZED, FEATURE };

    /*
     *  Pops one tape off the stack, re-enabling disabled nodes
     */
    void pop();

    /*
     *  Returns the fraction active / total nodes
     *  (to check how well disabling is working)
     */
    double utilization() const;

    /*  Indices of X, Y, Z coordinates */
    Clause::Id X, Y, Z;

    /*  Constants, unpacked from the tree at construction */
    std::map<Clause::Id, float> constants;

    /*  Map of variables (in terms of where they live in this Evaluator) to
     *  their ids in their respective Tree (e.g. what you get when calling
     *  Tree::var().id() */
    boost::bimap<Clause::Id, Tree::Id> vars;

    /*  Returns the total number of clauses (including X/Y/Z, variables, and
     *  constants, which aren't explicitly in the tape )  */
    size_t num_clauses;

protected:

    struct Subtape
    {
        /*  The tape itself, as a vector of clauses  */
        std::vector<Clause> t;

        /*  Root clause of the tape  */
        Clause::Id i;

        /*  These bounds are only valid if type == INTERVAL  */
        Interval::I X, Y, Z;
        Type type;
    };

    /*  Tape containing our opcodes in reverse order */
    std::list<Subtape> tapes;
    std::list<Subtape>::iterator tape;

    /*  Used when pushing into the tape  */
    std::vector<uint8_t> disabled;
    std::vector<Clause::Id> remap;

public:

    /*
     *  Pushes a new tape onto the stack, storing it in tape
     *
     *  fn must be a callable that tells us which side of each clause to keep
     *  t is a tape type
     *  r is the relevant region (or an empty region by default)
     */
    void push(std::function<Keep(Opcode::Opcode, Clause::Id,
                                 Clause::Id, Clause::Id)> fn,
              Type t, Region<3> r=Region<3>());

    /*
     *  Walks through the tape in bottom-to-top (reverse) order,
     *  calling an arbitrary function for every clause.
     *
     *  Returns the clause id of the tape's root
     */
    Clause::Id rwalk(
            std::function<void(Opcode::Opcode, Clause::Id,
                               Clause::Id, Clause::Id)> fn);
    Clause::Id rwalk(
            std::function<void(Opcode::Opcode, Clause::Id,
                               Clause::Id, Clause::Id)> fn, bool& abort);

    void  walk(std::function<void(Opcode::Opcode, Clause::Id,
                                  Clause::Id, Clause::Id)> fn, bool& abort);

    /*
     *  Walks up the tree until p is within the tape's region, then
     *  calls e.eval(p).  This is useful for evaluating a point when
     *  the tape may be pushed into a deeper interval.
     */
    template <class E, class T>
    T baseEval(E& e, const Eigen::Vector3f& p)
    {
        auto prev_tape = tape;

        // Walk up the tape stack until we find an interval-type tape
        // that contains the given point, or we hit the start of the stack
        while (tape != tapes.begin())
        {
            if (tape->type == Tape::INTERVAL &&
                p.x() >= tape->X.lower() && p.x() <= tape->X.upper() &&
                p.y() >= tape->Y.lower() && p.y() <= tape->Y.upper() &&
                p.z() >= tape->Z.lower() && p.z() <= tape->Z.upper())
            {
                break;
            }
            else
            {
                tape--;
            }
        }

        auto out = e.eval(p);
        tape = prev_tape;
        return out;
    }
};

}   // namespace Kernel
