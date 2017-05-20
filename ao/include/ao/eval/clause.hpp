#pragma once

#include "kernel/tree/opcode.hpp"

namespace Kernel {

/*
 *  A clause is used in an Evaluator to evaluate a tree
 */
struct Clause
{
    typedef uint32_t Id;

    /*
     *  Clause constructor
     */
    Clause(Opcode::Opcode op, uint32_t id, uint32_t a, uint32_t b)
        : op(op), id(id), a(a), b(b) {}

    /*  Opcode for this clause  */
    const Opcode::Opcode op;

    /*  Populated for operators with arguments */
    Id const id;
    Id const a;
    Id const b;
};

}   // namespace Kernel
