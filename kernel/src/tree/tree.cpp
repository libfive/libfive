#include <cassert>
#include <algorithm>

#include "ao/tree/tree.hpp"
#include "ao/tree/store.hpp"
#include "ao/tree/atom.hpp"
#include "ao/tree/token.hpp"

////////////////////////////////////////////////////////////////////////////////

Tree::Tree(Store* s, Token* root_token)
    : X(nullptr), Y(nullptr), Z(nullptr), root(nullptr)
{
    std::unordered_map<const Token*, Atom*> atoms;

    // Set flags to mark which tokens are used in the tree
    s->markFound(root_token);

    // Ensure that the base variables are all present in the tree and marked
    // as found (even if there wasn't a path to them from the root)
    s->X()->found = true;
    s->Y()->found = true;
    s->Z()->found = true;

    // Create base variables
    X = new Atom(OP_X);
    Y = new Atom(OP_Y);
    Z = new Atom(OP_Z);

    // Use matrix-transform variables to apply a mapping to the store
    const size_t MATRIX_ROWS = 3;
    rows.resize(MATRIX_ROWS);
    atoms[s->X()] = buildMatrixRow(0);
    atoms[s->Y()] = buildMatrixRow(1);
    atoms[s->Z()] = buildMatrixRow(2);

    // Flatten constants into the data array and constants vector
    constants.reserve(constants.size() + s->constants.size());
    for (auto c : s->constants)
    {
        if (c.second->found)
        {
            constants.push_back(new Atom(c.second, atoms));
        }
    }

    {   // Flatten operations into the vector of rows vectors
        assert(rows.size() == MATRIX_ROWS);
        rows.resize(MATRIX_ROWS + s->ops.size() - 1);
        auto row = rows.begin() + MATRIX_ROWS;

        // We skip the weight-0 operations, since we've already captured them
        // in the matrix transform and the X, Y, Z members
        for (auto itr = s->ops.begin() + 1; itr != s->ops.end(); ++itr, ++row)
        {
            assert(row != rows.end());
            for (auto b : *itr)
            {
                for (auto c : b)
                {
                    if (c.second->found)
                    {
                        row->push_back(new Atom(c.second, atoms));
                    }
                }
            }
        }
        assert(row == rows.end());
    }

    // Get the root atom from the root token
    assert(atoms.count(root_token) == 1);
    root = atoms[root_token];
}

Tree::~Tree()
{
    delete(X);
    delete(Y);
    delete(Z);

    for (auto c : constants)
    {
        delete c;
    }

    for (auto row : rows)
    {
        for (auto m : row)
        {
            delete m;
        }
    }

    for (auto m : matrix)
    {
        delete m;
    }
}


////////////////////////////////////////////////////////////////////////////////

Atom* Tree::buildMatrixRow(size_t i)
{
    assert(X != nullptr && Y != nullptr && Z != nullptr);
    assert(rows.size() == 3);

    // The matrix transform is of the form
    //     q' = a*x + b*y + c*z + d
    // (where q' is x', y', or z')
    //
    // It is implemented with three OP_MULs (a*x, b*y, c*z)
    // and three OP_ADDs (a*x + b*y, c*z + d, a*x + b*y + c*z + d)

    // The default matrix preserves X, Y, Z values
    Atom* a  = new Atom(i == 0 ? 1.0 : 0.0);
    Atom* b  = new Atom(i == 1 ? 1.0 : 0.0);
    Atom* c  = new Atom(i == 2 ? 1.0 : 0.0);
    Atom* d  = new Atom(0.0);

    Atom* ax = new Atom(OP_MUL, X, a);
    Atom* by = new Atom(OP_MUL, Y, b);
    Atom* cz = new Atom(OP_MUL, Z, c);

    Atom* ax_by = new Atom(OP_ADD, ax, by);
    Atom* cz_d  = new Atom(OP_ADD, cz, d);

    Atom* ax_by_cz_d = new Atom(OP_ADD, ax_by, cz_d);

    // Store relevant constant Atoms in the matrix array
    matrix[4*i]     = a;
    matrix[4*i + 1] = b;
    matrix[4*i + 2] = c;
    matrix[4*i + 3] = d;

    // Load matrix transform into the operator array
    rows[0].push_back(ax);
    rows[0].push_back(by);
    rows[0].push_back(cz);

    rows[1].push_back(ax_by);
    rows[1].push_back(cz_d);

    rows[2].push_back(ax_by_cz_d);

    return ax_by_cz_d;
}
