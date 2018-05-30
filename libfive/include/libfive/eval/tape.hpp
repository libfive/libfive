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

#include <vector>

#include <boost/bimap.hpp>
#include <Eigen/Eigen>

#include "libfive/eval/clause.hpp"
#include "libfive/eval/oracle.hpp"
#include "libfive/eval/interval.hpp"
#include "libfive/tree/tree.hpp"

#include "libfive/render/brep/region.hpp"

namespace Kernel {

class Tape
{
public:
    Tape(const Tree root);

    /*  Returned by evaluator types when pushing
     *  KEEP_BOTH is returned when that function call could have returned
     *      KEEP_A or KEEP_B (if the values were different);
     *  KEEP_ALWAYS is returned when no set of values would return KEEP_A/B
     *
     *  This distinction lets us track whether a tape has any selective
     *  clauses (e.g. min and max) left; when no such clauses exist, then
     *  pushing can be faster */
    enum Keep { KEEP_BOTH, KEEP_A, KEEP_B, KEEP_ALWAYS };

    /*  Different kind of tape pushes  */
    enum Type { UNKNOWN, INTERVAL, SPECIALIZED, FEATURE };

    /*
     *  Returns the fraction active / total nodes
     *  (to check how well disabling is working)
     */
    double utilization() const;

    /*  Constants, indexed by lhs in CONSTANT nodes */
    std::vector<float> constants;

    /*  Map of variables (in terms of where they live in this Evaluator) to
     *  their ids in their respective Tree (e.g. what you get when calling
     *  Tree::var().id() */
    std::vector<Tree::Id> vars;

    /*  Oracles are also unpacked from the tree at construction, and
     *  stored in this flat list.  The ORACLE opcode takes an index into
     *  this list and an index into the results array. */
    std::vector<std::unique_ptr<Oracle>> oracles;

    /*  Returns the total number of clauses (including X/Y/Z, oracles,
     *  variables, and constants)  */
    size_t num_clauses;

    /*  Returns the maximum number of active clauses at once, which is
     *  used to decide how much space to allocate for intermediate results
     *  in Evaluators.  Think of this has a register count for evaluation. */
    size_t num_slots;

protected:

    struct Subtape
    {
        /*  The tape itself, as a vector of clauses  */
        std::vector<Clause> t;

        /*  Memory mapping from Clause::Ids to slots */
        std::vector<unsigned> m;

        /*  These bounds are only valid if type == INTERVAL  */
        Interval::I X, Y, Z;
        Type type;

        /*  When dummy = 0, this is a usual tape push.
         *
         *  When dummy is >= 1, this is a subtape that doesn't
         *  contain any min / max nodes, so pushing isn't useful any more;
         *  instead, we increment dummy on push and decrement on pop (until
         *  it equals 1, at which point we pop as usual). */
        unsigned dummy=0;
    };

    /*  Tape containing our opcodes in reverse order */
    std::list<Subtape> tapes;
    std::list<Subtape>::iterator tape;

    /*  Used when pushing into the tape  */
    std::vector<uint8_t> disabled;
    std::vector<Clause::Id> remap;

    /*
     *  Pops one tape off the stack, re-enabling disabled nodes
     *  This is private because it will only be called by the Handle destructor
     */
    void pop();

    /*
     *  Certain types of clauses (e.g. ORACLES) use their lhs to store
     *  an index into a secondary array, rather than as a slot identifier.
     *  These clauses are handled differently in tape pushing.
     */
    static bool hasDummyChildren(Opcode::Opcode op);

    /*
     *  Populates tape->m with memory slot assignments
     */
    void assignSlots();

public:
    /*
     *  A Handle is an RAII object that undoes a push
     */
    class Handle
    {
    public:
        Handle() { /* Nothing to do here */ }
        Handle(Tape* tape) : tape(tape), type(PUSH) {}
        Handle(Tape* tape, std::list<Subtape>::iterator prev)
            : tape(tape), type(BASE), prev(prev) {}

        /*  Handles must be moved, not copy-constructed or assigned */
        Handle(Handle&&);
        Handle& operator=(Handle&&);

        ~Handle();
    protected:
        Tape* tape=nullptr;
        enum { NONE, PUSH, BASE, } type=NONE;

        /*  Used in BASE Handles as the value to reset the tape to */
        std::list<Subtape>::iterator prev;
    };

    /*
     *  Pushes a new tape onto the stack, storing it in tape
     *
     *  fn must be a callable that tells us which side of each clause to keep
     *  t is a tape type
     *  r is the relevant region (or an empty region by default)
     */
    Handle push(std::function<Keep(Opcode::Opcode, Clause::Id,
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
                               Clause::Id, Clause::Id)> fn, bool& abort);

    /*
     *  Inlined, faster version of rwalk
     */
    template <class T>
    Clause::Id rwalk(T& t)
    {
        for (auto itr = tape->t.rbegin(); itr != tape->t.rend(); ++itr)
        {
            t(itr->op, itr->id, itr->a, itr->b);
        }
        assert(tape->t.size() > 0);
        return tape->t.begin()->id;
    }

    void  walk(std::function<void(Opcode::Opcode, Clause::Id,
                                  Clause::Id, Clause::Id)> fn, bool& abort);

    /*
     *  Walks up the tape list until p is within the tape's region, then
     *  returns a Handle that restores the original tape.
     *
     *  This is useful for evaluating a point when  the tape may be pushed
     *  into a deeper interval, e.g. in dual contouring where points can
     *  be positioned outside of their parent cells.
     */
    Handle getBase(const Eigen::Vector3f& p);
};

}   // namespace Kernel
