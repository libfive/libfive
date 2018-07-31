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

#include <Eigen/Eigen>

#include "libfive/eval/clause.hpp"
#include "libfive/eval/interval.hpp"
#include "libfive/tree/tree.hpp"

#include "libfive/render/brep/region.hpp"
#include "libfive/oracle/oracle_context.hpp"

namespace Kernel {
class Deck; /*  Foward declaration */

class Tape
{
public:
    /*  Returned by evaluator types when pushing
     *  KEEP_BOTH is returned when that function call could have returned
     *      KEEP_A or KEEP_B (if the values were different);
     *  KEEP_ALWAYS is returned when no set of values would return KEEP_A/B
     *
     *  This distinction lets us track whether a tape has any selective
     *  clauses (e.g. min and max) left; when no such clauses exist, then
     *  pushing can be faster */
    enum Keep { KEEP_BOTH, KEEP_A, KEEP_B, KEEP_ALWAYS };

    /*  Different kind of tapes  */
    enum Type { BASE, INTERVAL, SPECIALIZED, FEATURE };

    typedef std::shared_ptr<Tape> Handle;

    /*  Returns tape length (used in unit tests to check for shrinkage) */
    size_t size() const { return t.size(); }

    /*  Returns the assigned context from this tape */
    std::shared_ptr<OracleContext> getContext(unsigned i) const
    {
        assert(i < contexts.size());
        return contexts[i];
    }

    /*  Stores a context */
    void pushContext(std::shared_ptr<OracleContext> c)
    {
        contexts.push_back(c);
    }

    /*  Checks to see whether we have oracle contexts assigned */
    bool hasContext() const
    {
        return contexts.size() > 0;
    }


protected:
    /*  The tape itself, as a vector of clauses  */
    std::vector<Clause> t;

    /*  OracleContext handles used to speed up oracle evaluation
     *  by letting them push into the tree as well. */
    std::vector<std::shared_ptr<OracleContext>> contexts;

    /*  Root clause of the tape  */
    Clause::Id i;

    /*  These bounds are only valid if type == INTERVAL  */
    Interval::I X, Y, Z;
    Type type;

    /*  If terminal is true, then the tape contains no min/max clauses
     *  so cannot be specialized further */
    bool terminal=false;

    /*  This is the parent tape, used in getBase() when we need
     *  to traverse up through the tape. */
    Handle parent;

public:
    /*
     *  Returns a new tape that is specialized with the given function.
     *
     *  fn must be a callable that tells us which side of each clause to keep
     *  t is a tape type
     *  r is the relevant region (or an empty region by default)
     */
    static Handle push(const Handle& tape, Deck& deck,
         std::function<Keep(Opcode::Opcode, Clause::Id,
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
    Clause::Id rwalk(T& fn)
    {
        for (auto itr = t.rbegin(); itr != t.rend(); ++itr)
        {
            fn(itr->op, itr->id, itr->a, itr->b);
        }
        return i;
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
    static Handle getBase(Handle tape, const Eigen::Vector3f& p);

    friend class Deck;
};

}   // namespace Kernel
