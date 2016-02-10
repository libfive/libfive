#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/eval/clause.hpp"

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

    // Set derivatives for X, Y, Z (since these never change)
    X->result.deriv(1, 0, 0);
    Y->result.deriv(0, 1, 0);
    Z->result.deriv(0, 0, 1);

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

float Evaluator::eval(float x, float y, float z)
{
    set(x, y, z, 0);
    return values(1)[0];
}

Interval Evaluator::eval(Interval x, Interval y, Interval z)
{
    set(x, y, z);
    return interval();
}

void Evaluator::set(Interval x, Interval y, Interval z)
{
    X->result.set(x);
    Y->result.set(y);
    Z->result.set(z);
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

#define EVAL_LOOP for (size_t i=0; i < count; ++i)
static void clause(Opcode op,
        const float* __restrict a, const float* __restrict b,
        float* __restrict out, size_t count)
{
    switch (op) {
        case OP_ADD:
            EVAL_LOOP
            out[i] = a[i] + b[i];
            break;
        case OP_MUL:
            EVAL_LOOP
            out[i] = a[i] * b[i];
            break;
        case OP_MIN:
            EVAL_LOOP
            out[i] = fmin(a[i], b[i]);
            break;
        case OP_MAX:
            EVAL_LOOP
            out[i] = fmax(a[i], b[i]);
            break;
        case OP_SUB:
            EVAL_LOOP
            out[i] = a[i] - b[i];
            break;
        case OP_DIV:
            EVAL_LOOP
            out[i] = a[i] / b[i];
            break;
        case OP_SQRT:
            EVAL_LOOP
            out[i] = sqrt(a[i]);
            break;
        case OP_NEG:
            EVAL_LOOP
            out[i] = -a[i];
            break;
        case OP_ABS:
            EVAL_LOOP
            out[i] = fabs(a[i]);
            break;
        case OP_A:
            EVAL_LOOP
            out[i] = a[i];
            break;
        case OP_B:
            EVAL_LOOP
            out[i] = b[i];
            break;
        case INVALID:
        case OP_CONST:
        case OP_MUTABLE:
        case OP_X:
        case OP_Y:
        case OP_Z:
        case LAST_OP: assert(false);
    }
}

static void clause(Opcode op,
        const float* __restrict av,  const float* __restrict adx,
        const float* __restrict ady, const float* __restrict adz,

        const float* __restrict bv,  const float* __restrict bdx,
        const float* __restrict bdy, const float* __restrict bdz,

        float* __restrict ov,  float* __restrict odx,
        float* __restrict ody, float* __restrict odz,
        size_t count)
{
    // Evaluate the base operations in a single pass
    clause(op, av, bv, ov, count);

    switch (op) {
        case OP_ADD:
            EVAL_LOOP
            {
                odx[i] = adx[i] + bdx[i];
                ody[i] = ady[i] + bdy[i];
                odz[i] = adz[i] + bdz[i];
            }
            break;
        case OP_MUL:
            EVAL_LOOP
            {   // Product rule
                odx[i] = av[i]*bdx[i] + adx[i]*bv[i];
                ody[i] = av[i]*bdy[i] + ady[i]*bv[i];
                odz[i] = av[i]*bdz[i] + adz[i]*bv[i];
            }
            break;
        case OP_MIN:
            EVAL_LOOP
            {
                if (av[i] < bv[i])
                {
                    odx[i] = adx[i];
                    ody[i] = ady[i];
                    odz[i] = adz[i];
                }
                else
                {
                    odx[i] = bdx[i];
                    ody[i] = bdy[i];
                    odz[i] = bdz[i];
                }
            }
            break;
        case OP_MAX:
            EVAL_LOOP
            {
                if (av[i] < bv[i])
                {
                    odx[i] = bdx[i];
                    ody[i] = bdy[i];
                    odz[i] = bdz[i];
                }
                else
                {
                    odx[i] = adx[i];
                    ody[i] = ady[i];
                    odz[i] = adz[i];
                }
            }
            break;
        case OP_SUB:
            EVAL_LOOP
            {
                odx[i] = adx[i] - bdx[i];
                ody[i] = ady[i] - bdy[i];
                odz[i] = adz[i] - bdz[i];
            }
            break;
        case OP_DIV:
            EVAL_LOOP
            {
                const float p = pow(bv[i], 2);
                odx[i] = (bv[i]*adx[i] - av[i]*bdx[i]) / p;
                ody[i] = (bv[i]*ady[i] - av[i]*bdy[i]) / p;
                odz[i] = (bv[i]*adz[i] - av[i]*bdz[i]) / p;
            }
            break;
        case OP_SQRT:
            EVAL_LOOP
            {
                if (av[i] < 0)
                {
                    odx[i] = 0;
                    ody[i] = 0;
                    odz[i] = 0;
                }
                else
                {
                    odx[i] = adx[i] / (2 * ov[i]);
                    ody[i] = ady[i] / (2 * ov[i]);
                    odz[i] = adz[i] / (2 * ov[i]);
                }
            }
            break;
        case OP_NEG:
            EVAL_LOOP
            {
                odx[i] = -adx[i];
                ody[i] = -ady[i];
                odz[i] = -adz[i];
            }
            break;
        case OP_ABS:
            EVAL_LOOP
            {
                if (av[i] < 0)
                {
                    odx[i] = -adx[i];
                    ody[i] = -ady[i];
                    odz[i] = -adz[i];
                }
                else
                {
                    odx[i] = adx[i];
                    ody[i] = ady[i];
                    odz[i] = adz[i];
                }
            }
            break;
        case OP_A:
            EVAL_LOOP
            {
                odx[i] = adx[i];
                ody[i] = ady[i];
                odz[i] = adz[i];
            }
            break;
        case OP_B:
            EVAL_LOOP
            {
                odx[i] = bdx[i];
                ody[i] = bdy[i];
                odz[i] = bdz[i];
            }
            break;
        case INVALID:
        case OP_CONST:
        case OP_MUTABLE:
        case OP_X:
        case OP_Y:
        case OP_Z:
        case LAST_OP: assert(false);
    }
}
#undef EVAL_LOOP

static Interval clause(Opcode op, const Interval& a, const Interval& b)
{
    switch (op) {
        case OP_ADD:
            return a + b;
        case OP_MUL:
            return a * b;
        case OP_MIN:
            return boost::numeric::min(a, b);
        case OP_MAX:
            return boost::numeric::max(a, b);
        case OP_SUB:
            return a - b;
        case OP_DIV:
            return a / b;
        case OP_SQRT:
            return sqrt(a);
        case OP_NEG:
            return -a;
        case OP_ABS:
            return boost::numeric::abs(a);
        case OP_A:
            return a;
        case OP_B:
            return b;
        case INVALID:
        case OP_CONST:
        case OP_MUTABLE:
        case OP_X:
        case OP_Y:
        case OP_Z:
        case LAST_OP: assert(false);
    }
    return Interval();
}

#ifdef __AVX__
const float* Evaluator::values(size_t count, bool vectorize)
{
    if (vectorize)
    {
        // Do vectorized evaluation here
        return nullptr;
    }
#else
const float* Evaluator::values(size_t count)
{
#endif
    for (const auto& row : rows)
    {
        for (size_t i=0; i < row.active; ++i)
        {
            auto op = row[i]->op;

            float* a = row[i]->a ? row[i]->a->result.f : nullptr;
            float* b = row[i]->b ? row[i]->b->result.f : nullptr;

            // Modify the opcode if parts of the tree are disabled
            if (a && row[i]->a->flags & CLAUSE_FLAG_DISABLED)
            {
                op = OP_B;
            }
            if (b && row[i]->b->flags & CLAUSE_FLAG_DISABLED)
            {
                op = OP_A;
            }

            clause(op, a, b, row[i]->result.f, count);
        }
    }
    return root->result.f;
}

#ifdef __AVX__
std::tuple<const float*, const float*,
           const float*, const float*> Evaluator::derivs(size_t count,
                                                         bool vectorize)
{
    if (vectorize)
    {
        // Do vectorized evaluation here
        return nullptr;
    }
#else
std::tuple<const float*, const float*,
           const float*, const float*> Evaluator::derivs(size_t count)
{
#endif
    for (const auto& row : rows)
    {
        for (size_t i=0; i < row.active; ++i)
        {
            auto op = row[i]->op;

            float* av  = row[i]->a ? row[i]->a->result.f  : nullptr;
            float* adx = row[i]->a ? row[i]->a->result.dx : nullptr;
            float* ady = row[i]->a ? row[i]->a->result.dy : nullptr;
            float* adz = row[i]->a ? row[i]->a->result.dz : nullptr;

            float* bv  = row[i]->b ? row[i]->b->result.f  : nullptr;
            float* bdx = row[i]->b ? row[i]->b->result.dx : nullptr;
            float* bdy = row[i]->b ? row[i]->b->result.dy : nullptr;
            float* bdz = row[i]->b ? row[i]->b->result.dz : nullptr;

            // Modify the opcode if parts of the tree are disabled
            if (av && row[i]->a->flags & CLAUSE_FLAG_DISABLED)
            {
                op = OP_B;
            }
            if (bv && row[i]->b->flags & CLAUSE_FLAG_DISABLED)
            {
                op = OP_A;
            }

            clause(op, av, adx, ady, adz,
                       bv, bdx, bdy, bdz,
                       row[i]->result.f, row[i]->result.dx,
                       row[i]->result.dy, row[i]->result.dz,
                   count);
        }
    }
    return {root->result.f, root->result.dx, root->result.dy, root->result.dz};
}

Interval Evaluator::interval()
{
    for (const auto& row : rows)
    {
        for (size_t i=0; i < row.active; ++i)
        {
            auto op = row[i]->op;

            Interval a = row[i]->a ? row[i]->a->result.i : Interval();
            Interval b = row[i]->b ? row[i]->b->result.i : Interval();

            // Modify the opcode if parts of the tree are disabled
            if (row[i]->a && row[i]->a->flags & CLAUSE_FLAG_DISABLED)
            {
                op = OP_B;
            }
            if (row[i]->b && row[i]->b->flags & CLAUSE_FLAG_DISABLED)
            {
                op = OP_A;
            }

            row[i]->result.i = clause(op, a, b);
        }
    }
    return root->result.i;
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

////////////////////////////////////////////////////////////////////////////////

#ifdef __AVX__
void Evaluator::packAVX()
{
    X->result.packAVX();
    Y->result.packAVX();
    Z->result.packAVX();
}

const float* Evaluator::unpackAVX()
{
    root->result.unpackAVX();
    return root->result.f;
}
#endif
