#include "tree.h"
#include "store.h"
#include "atom.h"
#include "token.h"

Tree::Tree(Store* s, Token* root_token)
    : X(nullptr), Y(nullptr), Z(nullptr), root(nullptr), data(nullptr)
{
    {   // Allocate space for all of the atoms in the tree
        size_t count = s->constants.size();
        for (auto a : s->ops)
        {
            for (auto b : a)
            {
                count += b.size();
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
            constants.push_back(new(&data[i++]) Atom(c.second));
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
                    row->push_back(new(&data[i++]) Atom(c.second));
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

#define EVAL_LOOP(M, R, F) \
for (size_t i=0; i < count; ++i) { M->result.R[i] = F; } break;

#define EVAL_FUNC(T, R) \
std::vector<T> Tree::eval(const std::vector<T>& x,                                  \
                          const std::vector<T>& y,                                  \
                          const std::vector<T>& z)                                  \
{                                                                                   \
    setPos(x, y, z);                                                                \
                                                                                    \
    const size_t count = x.size();                                                  \
    for (const auto& row : rows)                                                    \
    {                                                                               \
        for (auto& m : row)                                                         \
        {                                                                           \
            switch (m->op) {                                                        \
            case OP_ADD:                                                            \
                EVAL_LOOP(m, R, m->a->result.R[i] + m->b->result.R[i])              \
            case OP_MUL:                                                            \
                EVAL_LOOP(m, R, m->a->result.R[i] * m->b->result.R[i])              \
            case OP_MIN:                                                            \
                EVAL_LOOP(m, R, std::min(m->a->result.R[i], m->b->result.R[i]))     \
            case OP_MAX:                                                            \
                EVAL_LOOP(m, R, std::max(m->a->result.R[i], m->b->result.R[i]))     \
            case OP_SUB:                                                            \
                EVAL_LOOP(m, R, m->a->result.R[i] - m->b->result.R[i])              \
            case OP_DIV:                                                            \
                EVAL_LOOP(m, R, m->a->result.R[i] / m->b->result.R[i])              \
            case OP_SQRT:                                                           \
                EVAL_LOOP(m, R, sqrt(m->a->result.R[i]))                            \
            case OP_NEG:                                                            \
                EVAL_LOOP(m, R, -m->a->result.R[i])                                 \
            case INVALID:                                                           \
            case OP_CONST:                                                          \
            case OP_X:                                                              \
            case OP_Y:                                                              \
            case OP_Z:                                                              \
            case LAST_OP: assert(false);                                            \
            }                                                                       \
        }                                                                           \
    }                                                                               \
    return std::vector<T>(root->result.R, root->result.R + x.size());               \
}                                                                                   \

EVAL_FUNC(double, d);
EVAL_FUNC(Interval, i);

////////////////////////////////////////////////////////////////////////////////

void Tree::modeDouble(size_t count)
{
    assert(count <= ATOM_ARRAY_SIZE);
    for (auto c : constants)
    {
        c->result.set(std::vector<double>(count ? count : ATOM_ARRAY_SIZE,
                                          c->value));
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

template <class T>
void Tree::setPos(const std::vector<T>& x,
                  const std::vector<T>& y,
                  const std::vector<T>& z)
{
    assert(x.size() == y.size() && x.size() == z.size());

    X->result.set(x);
    Y->result.set(y);
    Z->result.set(z);
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

