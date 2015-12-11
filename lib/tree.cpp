#include <algorithm>

#include "tree.h"
#include "store.h"
#include "atom.h"
#include "token.h"

Tree::Tree(Store* s, Token* root_token)
    : X(nullptr), Y(nullptr), Z(nullptr), root(nullptr), data(nullptr)
{
    // Set flags to mark which tokens are used in the tree
    s->markFound(root_token);

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

double Tree::eval(double x, double y, double z)
{
    return eval(std::vector<double>(1, x),
                std::vector<double>(1, y),
                std::vector<double>(1, z))[0];
}

Interval Tree::eval(Interval x, Interval y, Interval z)
{
    return eval(std::vector<Interval>(1, x),
                std::vector<Interval>(1, y),
                std::vector<Interval>(1, z))[0];
}

////////////////////////////////////////////////////////////////////////////////

#define EVAL_LOOP(M, R, F) \
for (size_t i=0; i < count; ++i) { M->result.R[i] = F; } break;

#define LOAD_IF(V, VS, I, C) \
if (V)                                  \
{                                       \
    V->result.set(&VS[I], C);           \
}

#define EVAL_FUNC(T, R, C) \
std::vector<T> Tree::eval(const std::vector<T>& x,                          \
                          const std::vector<T>& y,                          \
                          const std::vector<T>& z)                          \
{                                                                           \
    assert(x.size() == y.size() && x.size() == z.size());                   \
                                                                            \
    size_t remaining = x.size();                                            \
                                                                            \
    std::vector<T> out;                                                     \
    out.resize(remaining);                                                  \
    size_t index = 0;                                                       \
                                                                            \
    while (remaining)                                                       \
    {                                                                       \
        const size_t count = std::min(remaining, C);                        \
                                                                            \
        LOAD_IF(X, x, index, count);                                        \
        LOAD_IF(Y, y, index, count);                                        \
        LOAD_IF(Z, z, index, count);                                        \
                                                                            \
        for (const auto& row : rows)                                        \
        {                                                                   \
            for (auto& m : row)                                             \
            {                                                               \
                switch (m->op) {                                            \
                case OP_ADD:                                                \
                    EVAL_LOOP(m, R, m->a->result.R[i] + m->b->result.R[i])  \
                case OP_MUL:                                                \
                    EVAL_LOOP(m, R, m->a->result.R[i] * m->b->result.R[i])  \
                case OP_MIN:                                                \
                    EVAL_LOOP(m, R, std::min(m->a->result.R[i],             \
                                             m->b->result.R[i]))            \
                case OP_MAX:                                                \
                    EVAL_LOOP(m, R, std::max(m->a->result.R[i],             \
                                             m->b->result.R[i]))            \
                case OP_SUB:                                                \
                    EVAL_LOOP(m, R, m->a->result.R[i] - m->b->result.R[i])  \
                case OP_DIV:                                                \
                    EVAL_LOOP(m, R, m->a->result.R[i] / m->b->result.R[i])  \
                case OP_SQRT:                                               \
                    EVAL_LOOP(m, R, sqrt(m->a->result.R[i]))                \
                case OP_NEG:                                                \
                    EVAL_LOOP(m, R, -m->a->result.R[i])                     \
                case INVALID:                                               \
                case OP_CONST:                                              \
                case OP_X:                                                  \
                case OP_Y:                                                  \
                case OP_Z:                                                  \
                case LAST_OP: assert(false);                                \
                }                                                           \
            }                                                               \
        }                                                                   \
        remaining -= count;                                                 \
        std::copy(root->result.R, root->result.R + count, &out[index]);     \
        index += count;                                                     \
    }                                                                       \
    return out;                                                             \
}                                                                           \

EVAL_FUNC(double, d, ATOM_DOUBLE_COUNT);
EVAL_FUNC(Interval, i, ATOM_INTERVAL_COUNT);

#undef EVAL_LOOP
#undef EVAL_FUNC
#undef LOAD_IF

////////////////////////////////////////////////////////////////////////////////

void Tree::modeDouble(size_t count)
{
    count = std::min(count, ATOM_DOUBLE_COUNT);
    for (auto c : constants)
    {
        c->result.set(std::vector<double>(
                    count ? count : ATOM_DOUBLE_COUNT,
                    c->value));
    }
}

void Tree::modeInterval(size_t count)
{
    count = std::min(count, ATOM_INTERVAL_COUNT);
    for (auto c : constants)
    {
        c->result.set(std::vector<Interval>(
                    count ? count : ATOM_INTERVAL_COUNT,
                    Interval(c->value, c->value)));
    }
}

////////////////////////////////////////////////////////////////////////////////

void Tree::setFlag(uint8_t flag)
{
    for (auto& r : rows)
    {
        r.setFlag(flag);
    }
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

    std::swap((*this)[i], (*this)[--active]);
    disabled.top()++;
}

////////////////////////////////////////////////////////////////////////////////

