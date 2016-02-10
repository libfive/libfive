#pragma once

#include <unordered_map>

#include "ao/kernel/tree/opcode.hpp"
#include "ao/kernel/eval/result.hpp"

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
    void disable() {   setFlag(CLAUSE_FLAG_DISABLED); }
    void enable()  { clearFlag(CLAUSE_FLAG_DISABLED); }

protected:
    /*  Opcode for this clause  */
    const Opcode op;

    /*  Populated for OP_CONST clause */
    const float value;

    /*  Populated for OP_MUTABLE clause  */
    float mutable_value;

    /*  Flags are set during evaluation for various purposes  */
    uint8_t flags=0;

    /*  Populated for operators with arguments */
    Clause* const a;
    Clause* const b;

    /*  Store pointers to various result arrays for fast evaluation
     *  (since a and b don't change, no need to check them for null on
     *   every iteration through an evaluation)*/
#ifdef __AVX__
    struct ResultPtrs { float *f, *dx, *dy, *dz;
                        __m256 *mf, *mdx, *mdy, *mdz; };
#else
    struct ResultPtrs { float *f, *dx, *dy, *dz; };
#endif
    struct { ResultPtrs a;
             ResultPtrs b; } ptrs;

    /*  Results are stored in a struct */
    Result result;

    friend class Evaluator;
};
