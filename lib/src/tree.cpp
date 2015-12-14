#include <algorithm>

#include "tree.hpp"
#include "store.hpp"
#include "atom.hpp"
#include "token.hpp"

Tree::Tree(Store* s, Token* root_token)
    : X(nullptr), Y(nullptr), Z(nullptr), root(nullptr),
      mode(MODE_NONE), data(nullptr)
{
    // Set flags to mark which tokens are used in the tree
    s->markFound(root_token);

    // Ensure that the base variables are all present in the tree
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
        data = static_cast<Atom*>(malloc(sizeof(Atom) * count));
    }

    // Expand rows to fit all of the token weights
    rows.resize(s->ops.size());

    {   // Flatten constants into the data array and constants vector
        size_t i = 0;
        constants.reserve(s->constants.size());
        for (auto c : s->constants)
        {
            if (c.second->found)
            {
                constants.push_back(new(&data[i++]) Atom(c.second));
            }
        }

        // Flatten operations into the data array and the list of rows vectors
        // row is an iterator that points to the current row
        auto row = rows.begin();
        for (auto a : s->ops)
        {
            for (auto b : a)
            {
                for (auto c : b)
                {
                    if (c.second->found)
                    {
                        row->push_back(new(&data[i++]) Atom(c.second));
                    }
                }
            }
            row++;
        }
    }

    // Extract variables from weight-0 row then erase it
    assert(rows.front().size() <= 3);
    for (auto a : rows.front())
    {
        if (a->op == OP_X)
        {
            assert(X == nullptr);
            X = a;
        }
        else if (a->op == OP_Y)
        {
            assert(Y == nullptr);
            Y = a;
        }
        else if (a->op == OP_Z)
        {
            assert(Z == nullptr);
            Z = a;
        }
    }
    assert(X != nullptr && Y != nullptr && Z != nullptr);
    rows.pop_front();

    // Set the active node count in every row to the number of atoms
    for (auto& r : rows)
    {
        r.active = r.size();
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

void Tree::setFlag(uint8_t flag)
{
    for (auto& r : rows)
    {
        r.setFlag(flag);
    }
}

const double* Tree::eval(const Region& r)
{
    assert(r.voxels() <= ATOM_DOUBLE_COUNT);

    setMode<double>();

    size_t index = 0;
    r.forEach([&](size_t i, size_t j, size_t k)
            {
                X->result.set(r.X.pos(i), index);
                Y->result.set(r.Y.pos(j), index);
                Z->result.set(r.Z.pos(k), index);
                index++;
            });

    evalCore<double>(r.voxels());
    return root->result.ptr<double>();
}

////////////////////////////////////////////////////////////////////////////////

void Tree::Row::setFlag(uint8_t flag)
{
    for (size_t i=0; i < active; ++i)
    {
        (*this)[i]->flags |= flag;
    }
}

void Tree::Row::push()
{
    disabled.push(0);
}

void Tree::Row::pop()
{
    active += disabled.top();
    disabled.pop();
}

void Tree::Row::disable(size_t i)
{
    assert(i < active);
    assert(disabled.size() > 0);

    std::swap((*this)[i], (*this)[--active]);
    disabled.top()++;
}
