#include <algorithm>

#include "ao/core/tree.hpp"
#include "ao/core/store.hpp"
#include "ao/core/atom.hpp"
#include "ao/core/token.hpp"

////////////////////////////////////////////////////////////////////////////////

#define NEW_ATOM(...) new (ptr++) Atom(__VA_ARGS__)

Tree::Tree(Store* s, Token* root_token)
    : X(nullptr), Y(nullptr), Z(nullptr), root(nullptr),
      data(nullptr), ptr(nullptr)
{
    // Set flags to mark which tokens are used in the tree
    s->markFound(root_token);

    // Ensure that the base variables are all present in the tree and marked
    // as found (even if there wasn't a path to them from the root)
    s->X()->found = true;
    s->Y()->found = true;
    s->Z()->found = true;

    {   // Count up active tokens and allocate space for them
        size_t count = std::count_if(s->constants.begin(), s->constants.end(),
                [](decltype(s->constants)::value_type t)
                { return t.second->found; });

        for (auto a : s->ops)
        {
            for (auto b : a)
            {
                count += std::count_if(b.begin(), b.end(),
                        [](const decltype(b)::value_type& t)
                        { return t.second->found; });
            }
        }

        // Allocate extra space for the matrix transform
        const size_t MATRIX_TRANFORM_ATOMS = 3*12;
        count += MATRIX_TRANFORM_ATOMS;

        data = static_cast<Atom*>(malloc(sizeof(Atom) * count));
        ptr = data;
    }

    // Create base variables
    X = NEW_ATOM(OP_X);
    Y = NEW_ATOM(OP_Y);
    Z = NEW_ATOM(OP_Z);

    // Use matrix-transform variables to apply a mapping to the store
    const size_t MATRIX_ROWS = 3;
    rows.resize(MATRIX_ROWS);
    s->X()->atom = buildMatrixRow(0);
    s->Y()->atom = buildMatrixRow(1);
    s->Z()->atom = buildMatrixRow(2);

    // Flatten constants into the data array and constants vector
    constants.reserve(constants.size() + s->constants.size());
    for (auto c : s->constants)
    {
        if (c.second->found)
        {
            constants.push_back(NEW_ATOM(c.second));
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
                        row->push_back(NEW_ATOM(c.second));
                    }
                }
            }
        }
        assert(row == rows.end());
    }

    // Set the active count in every row to the number of atoms
    for (auto& r : rows)
    {
        r.setSize();
    }

    // Get the root atom from the root token
    assert(root_token->atom != nullptr);
    root = root_token->atom;
}

Tree::~Tree()
{
    free(data);
}

////////////////////////////////////////////////////////////////////////////////

void Tree::setMatrix(const glm::mat4& m)
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

Atom* Tree::buildMatrixRow(size_t i)
{
    assert(data != nullptr);
    assert(X != nullptr && Y != nullptr && Z != nullptr);
    assert(rows.size() == 3);

    // The matrix transform is of the form
    //     q' = a*x + b*y + c*z + d
    // (where q' is x', y', or z')
    //
    // It is implemented with three OP_MULs (a*x, b*y, c*z)
    // and three OP_ADDs (a*x + b*y, c*z + d, a*x + b*y + c*z + d)

    // The default matrix preserves X, Y, Z values
    Atom* a  = NEW_ATOM(i == 0 ? 1.0 : 0.0);
    Atom* b  = NEW_ATOM(i == 1 ? 1.0 : 0.0);
    Atom* c  = NEW_ATOM(i == 2 ? 1.0 : 0.0);
    Atom* d  = NEW_ATOM(0.0);

    Atom* ax = NEW_ATOM(OP_MUL, X, a);
    Atom* by = NEW_ATOM(OP_MUL, Y, b);
    Atom* cz = NEW_ATOM(OP_MUL, Z, c);

    Atom* ax_by = NEW_ATOM(OP_ADD, ax, by);
    Atom* cz_d  = NEW_ATOM(OP_ADD, cz, d);

    Atom* ax_by_cz_d = NEW_ATOM(OP_ADD, ax_by, cz_d);

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

////////////////////////////////////////////////////////////////////////////////

void Tree::push()
{
    // Walk up the tree, marking every atom with ATOM_FLAG_IGNORED
    for (const auto& row : rows)
    {
        for (size_t i=0; i < row.active; ++i)
        {
            row[i]->setFlag(ATOM_FLAG_IGNORED);
        }
    }

    // Clear the IGNORED flag on the root
    root->clearFlag(ATOM_FLAG_IGNORED);

    // Walk down the tree, clearing IGNORED flags as appropriate
    // and disabling atoms that still have IGNORED flags set.
    for (auto itr = rows.rbegin(); itr != rows.rend(); ++itr)
    {
        itr->push();
    }
}

void Tree::pop()
{
    for (auto& row : rows)
    {
        row.pop();
    }
}

////////////////////////////////////////////////////////////////////////////////

const double* Tree::eval(const Region& r)
{
    assert(r.voxels() <= Result::count<double>());

    size_t index = 0;

    // Flatten the region in a particular order
    // (which needs to be obeyed by anything unflattening results)
    REGION_ITERATE_XYZ(r)
    {
        X->result.set(r.X.pos(i), index);
        Y->result.set(r.Y.pos(j), index);
        Z->result.set(r.Z.pos(r.Z.size - k - 1), index);
        index++;
    }

    return evalCore<double>(r.voxels());
}

////////////////////////////////////////////////////////////////////////////////

std::string Tree::toShader() const
{
    std::unordered_map<const Atom*, size_t> atoms;

    // Write shader's header
    std::string out = R"EOF(
#version 330

out vec4 fragColor;
in vec2 frag_pos;

uniform int nk;
uniform float dz;

void main()
{
    float x = frag_pos.x;
    float y = frag_pos.y;
    float z = 0.0f;

)EOF";

    // Build shader line-by-line from the active atoms
    size_t index = 1;
    atoms[root] = 0;
    for (const auto& row : rows)
    {
        for (size_t i=0; i < row.active; ++i)
        {
            out += row[i]->toShader(index++, &atoms);
        }
    }

    // Append the shader's footer
    out += R"EOF(
    if (m0 <=0)
    {
        fragColor = vec4(1.0f, 1.0f, 1.0f, 1.0f);
    }
    else
    {
        fragColor = vec4(0.0f, 0.0f, 0.0f, 1.0f);
    }
}
)EOF";

    return out;
}
