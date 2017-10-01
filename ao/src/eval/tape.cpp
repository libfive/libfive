#include <unordered_map>

#include "ao/eval/tape.hpp"

namespace Kernel {

Tape::Tape(const Tree root)
{
    auto flat = root.ordered();

    // Helper function to create a new clause in the data array
    // The dummy clause (0) is mapped to the first result slot
    std::unordered_map<Tree::Id, Clause::Id> clauses = {{nullptr, 0}};
    Clause::Id id = flat.size();

    // Store the total number of clauses plus one,
    // because the clause with id = 0 is a dummy clause (?)
    num_clauses = id + 1;

    // Helper function to make a new function
    std::list<Clause> tape_;
    auto newClause = [&clauses, &id, &tape_](const Tree::Id t)
    {
        tape_.push_front(
                {t->op,
                 id,
                 clauses.at(t->lhs.get()),
                 clauses.at(t->rhs.get())});
    };

    // Write the flattened tree into the tape!
    for (const auto& m : flat)
    {
        // Normal clauses end up in the tape
        if (m->rank > 0)
        {
            newClause(m.id());
        }
        // For constants and variables, record their values so
        // that we can store those values in the result array
        else if (m->op == Opcode::CONST)
        {
            constants[id] = m->value;
        }
        else if (m->op == Opcode::VAR)
        {
            vars.left.insert({id, m.id()});
        }
        else
        {
            assert(m->op == Opcode::VAR_X ||
                   m->op == Opcode::VAR_Y ||
                   m->op == Opcode::VAR_Z);
        }
        clauses[m.id()] = id--;
    }
    assert(id == 0);

    //  Move from the list tape to a more-compact vector tape
    tapes.push_back(Subtape());
    tape = tapes.begin();
    for (auto& t : tape_)
    {
        tape->t.push_back(t);
    }

    // Make sure that X, Y, Z have been allocated space
    std::vector<Tree> axes = {Tree::X(), Tree::Y(), Tree::Z()};
    for (auto a : axes)
    {
        if (clauses.find(a.id()) == clauses.end())
        {
            clauses[a.id()] = clauses.size();
        }
    }

    // Allocate enough memory for all the clauses
    disabled.resize(clauses.size() + 1);
    remap.resize(clauses.size() + 1);

    // Save X, Y, Z ids
    X = clauses.at(axes[0].id());
    Y = clauses.at(axes[1].id());
    Z = clauses.at(axes[2].id());

    // Store the index of the tree's root
    assert(clauses.at(root.id()) == 1);
    tape->i = clauses.at(root.id());
};

void Tape::pop()
{
    assert(tape != tapes.begin());
    tape--;
}

double Tape::utilization() const
{
    return tape->t.size() / double(tapes.front().t.size());
}

}   // namespace Kernel
