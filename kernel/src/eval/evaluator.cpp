#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/eval/evaluator.hpp"

////////////////////////////////////////////////////////////////////////////////

Evaluator::Evaluator(const Tree* tree)
{
    std::unordered_map<const Atom*, Clause*> clauses;

    // Count up the number of Atoms in the Tree and allocate space for them
    size_t count = 3                // X, Y, Z
         + tree->matrix.size()      // Transform matrix
         + tree->constants.size();  // Constants
    for (auto r : tree->rows)
    {
        count += r.size();
    }
    data = static_cast<Clause*>(malloc(sizeof(Clause) * count));
    ptr = data;

    // Load constants into the array first
    for (auto m : tree->constants)
    {
        constants.push_back(newClause(m, clauses));
    }

    // Create base clauses X, Y, Z
    X = newClause(tree->X, clauses);
    Y = newClause(tree->Y, clauses);
    Z = newClause(tree->Z, clauses);

    // Create matrix clauses
    assert(tree->matrix.size() == matrix.size());
    for (size_t i=0; i < tree->matrix.size(); ++i)
    {
        matrix[i] = newClause(tree->matrix[i], clauses);
    }

    // Finally, create the rest of the Tree's clauses
    for (auto r : tree->rows)
    {
        rows.push_back(Row());
        for (auto m : r)
        {
            rows.back().push_back(newClause(m, clauses));
        }
        rows.back().setSize();
    }

    assert(clauses[tree->root]);
    root = clauses[tree->root];
}

Evaluator::Evaluator(const Tree* t, const glm::mat4& m)
    : Evaluator(t)
{
    setMatrix(m);
}

Evaluator::~Evaluator()
{
    free(data);
}

////////////////////////////////////////////////////////////////////////////////

void Evaluator::setMatrix(const glm::mat4& m)
{
    size_t index = 0;
    for (int i=0; i < 3; ++i)
    {
        for (int j=0; j < 4; ++j)
        {
            assert(matrix[index]->op == OP_MUTABLE);
            matrix[index++]->mutable_value = m[j][i];
        }
    }

    for (auto m : matrix)
    {
        m->result.fill(m->mutable_value);
    }
}

////////////////////////////////////////////////////////////////////////////////

void Evaluator::push()
{
    // Walk up the tree, marking every atom with ATOM_FLAG_IGNORED
    for (const auto& row : rows)
    {
        for (size_t i=0; i < row.active; ++i)
        {
            row[i]->setFlag(CLAUSE_FLAG_IGNORED);
        }
    }

    // Clear the IGNORED flag on the root
    root->clearFlag(CLAUSE_FLAG_IGNORED);

    // Walk down the tree, clearing IGNORED flags as appropriate
    // and disabling atoms that still have IGNORED flags set.
    for (auto itr = rows.rbegin(); itr != rows.rend(); ++itr)
    {
        itr->push();
    }
}

void Evaluator::pop()
{
    for (auto& row : rows)
    {
        row.pop();
    }
}

////////////////////////////////////////////////////////////////////////////////

const float* Evaluator::eval(const Subregion& r)
{
    assert(r.voxels() <= Result::count<float>());

    size_t index = 0;

    // Flatten the region in a particular order
    // (which needs to be obeyed by anything unflattening results)
    SUBREGION_ITERATE_XYZ(r)
    {
        X->result.set<float>(r.X.pos(i), index);
        Y->result.set<float>(r.Y.pos(j), index);
        Z->result.set<float>(r.Z.pos(r.Z.size - k - 1), index);
        index++;
    }

#ifdef __AVX__
    X->result.packAVX();
    Y->result.packAVX();
    Z->result.packAVX();

    evalCore<__m256>(r.voxels());

    root->result.unpackAVX();
#else
    evalCore<float>(r.voxels());
#endif

    return root->result.ptr<float>();
}

////////////////////////////////////////////////////////////////////////////////

Clause* Evaluator::newClause(const Atom* m,
                             std::unordered_map<const Atom*, Clause*>& clauses)
{
    return new (ptr++) Clause(m, clauses);
}

////////////////////////////////////////////////////////////////////////////////

double Evaluator::utilization() const
{
    double total = 0;
    double active = 0;

    for (const auto& r : rows)
    {
        total += r.size();
        active += r.active;
    }

    return active / total;
}
