#pragma once

#include <unordered_map>

#include "ao/tree/opcode.hpp"
#include "ao/eval/result.hpp"

class Atom;

#define CLAUSE_FLAG_IGNORED  1
#define CLAUSE_FLAG_DISABLED 2

/*
 *  A clause is used in an Evaluator to evaluate a tree
 */
class Clause
{
public:
    explicit Clause(const Atom* m,
                    std::unordered_map<const Atom*, Clause*>& clauses);

    /*
     *  Flag manipulation functions
     */
    void setFlag(uint8_t f)     { flags |=  f; }
    void clearFlag(uint8_t f)   { flags &= ~f; }
    void clearFlags()           { flags  =  0; }

    /*
     *  If the CLAUSE_FLAG_IGNORED flag is set:
     *      Saves the interval result to mutable_value
     *      Clears the flag
     *      Returns true.
     *
     *  Otherwise, propagates the IGNORED flag to its children,
     *  properly handling min and max operations (which may only
     *  leave one child active), returning false.
     */
    bool checkDisabled();

    /*
     *  Stores the most recent Interval evaluation in the mutable_value slot
     *  and set CLAUSE_FLAG_DISABLED
     */
    void disable();
    void enable();

    /*
     *  Returns results.get<T>(i) if this clause is enabled,
     *  T(mutable_value) otherwise
     */
    template <class T>
    T get(size_t index)
    {
        if (flags & CLAUSE_FLAG_DISABLED)
        {
            return T(mutable_value);
        }
        else
        {
            return result.get<T>(index);
        }
    }

protected:
    /*  Opcode for this clause  */
    const Opcode op;

    /*  Populated for OP_CONST clause */
    const double value;

    /*  Populated for OP_MUTABLE clause  */
    double mutable_value;

    /*  Flags are set during evaluation for various purposes  */
    uint8_t flags=0;

    /*  Populated for operators with arguments */
    Clause* const a;
    Clause* const b;

    /*  Populated for conditional operators  */
    Clause* const cond;

    /*  Results are stored in a struct */
    Result result;

    friend class Evaluator;
};
