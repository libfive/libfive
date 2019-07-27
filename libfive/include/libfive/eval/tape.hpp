/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <vector>
#include <memory>

#include <Eigen/Eigen>

#include "libfive/eval/interval.hpp"
#include "libfive/eval/clause.hpp"
#include "libfive/tree/opcode.hpp"

#include "libfive/oracle/oracle_context.hpp"
#include "libfive/inline.hpp"

namespace libfive {

/*  Foward declarations */
template <unsigned N> class Region;
class Deck;

class Tape : public std::enable_shared_from_this<Tape>
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
    std::shared_ptr<OracleContext> getContext(unsigned i) const;

    /*  Returns whether this tape is marked as terminal.
     *
     *  A terminal tape has no operations that can be simplified
     *  (e.g. no min / max clauses), so pushing is a no-op.
     */
    bool isTerminal() const { return terminal; }

    std::vector<uint32_t>::const_reverse_iterator rbegin() const
    { return t.crbegin(); }

    std::vector<uint32_t>::const_reverse_iterator rend() const
    { return t.crend(); }

    LIBFIVE_INLINE
    static Clause next(std::vector<uint32_t>::const_reverse_iterator& itr)
    {
        const auto op = static_cast<Opcode::Opcode>(*itr & OPCODE_MASK);
        const auto args = (op == Opcode::ORACLE) ? 1 : Opcode::args(op);
        const uint32_t id = *(itr + 1);
        uint32_t const* const a = &*(itr + 2);
        uint32_t const* const b = &*(itr + 3);
        itr += args + 2;
        return Clause { op, id, a, b };
    }

    LIBFIVE_INLINE
    static Clause next(std::vector<uint32_t>::const_iterator& itr)
    {
        uint32_t const* b = &*itr;

        while (!(*itr & OPCODE_FLAG)) {
            ++itr;
        }

        static uint32_t z = 0;
        const auto op = static_cast<Opcode::Opcode>(*itr & OPCODE_MASK);
        const auto args = (op == Opcode::ORACLE) ? 1 : Opcode::args(op);
        const uint32_t id = *(itr - 1);
        uint32_t const* const a = (args >= 1) ? &*(itr - 2) : &z;
        if (args < 2) {
            b = &z;
        }

        ++itr;
        return Clause { op, id, a, b };
    }

    uint32_t root() const { return i; }

    const static uint32_t OPCODE_FLAG = (1U << 31);
    const static uint32_t OPCODE_MASK = OPCODE_FLAG - 1;

protected:
    /*  The tape itself, as a vector of raw data.  */
    std::vector<uint32_t> t;

    /*  OracleContext handles used to speed up oracle evaluation
     *  by letting them push into the tree as well. */
    std::vector<std::shared_ptr<OracleContext>> contexts;

    /*  Root clause of the tape  */
    uint32_t i;

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
    using KeepFunction = std::function<Keep(Opcode::Opcode, uint32_t,
                                            uint32_t, uint32_t)>;
    Handle push(Deck& deck, KeepFunction fn, Type t);
    Handle push(Deck& deck, KeepFunction fn, Type t, const Region<3>& r);

    /*
     *  Walks up the tape list until p is within the tape's region, then
     *  returns a Handle that restores the original tape.
     *
     *  This is useful for evaluating a point when  the tape may be pushed
     *  into a deeper interval, e.g. in dual contouring where points can
     *  be positioned outside of their parent cells.
     */
    Handle getBase(const Eigen::Vector3f& p);
    Handle getBase(const Region<3>& r);

    friend class Deck;
};

}   // namespace libfive
