#pragma once

#include <boost/numeric/interval.hpp>

#include "opcode.h"
#include "gradient.h"

#define ATOM_ARRAY_SIZE 64

/*
 *  An Atom represent an operation in a math tree.
 */
class Atom
{
    Atom(Opcode op, Atom* a=nullptr, Atom* b=nullptr);
    Atom(double v);

protected:
    const Opcode op;

    /*  Populated for OP_CONST atoms */
    const double value;

    /*  Populated for operators with arguments */
    Atom* const a;
    Atom* const b;

    /*  Results are stored in this union */
    union Result {
        Result() { /* Provide default constructor */ }

        double d[ATOM_ARRAY_SIZE];
        boost::numeric::interval<double> i[ATOM_ARRAY_SIZE/2];
        Gradient g[ATOM_ARRAY_SIZE / 4];
    } result;

    friend class Tree;
};
