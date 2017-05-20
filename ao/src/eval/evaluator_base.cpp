#include <numeric>
#include <memory>
#include <cmath>

#include <glm/gtc/matrix_inverse.hpp>

#include "ao/tree/cache.hpp"
#include "ao/tree/tree.hpp"
#include "ao/eval/evaluator_base.hpp"
#include "ao/eval/clause.hpp"

namespace Kernel {

////////////////////////////////////////////////////////////////////////////////

EvaluatorBase::EvaluatorBase(const Tree root, const glm::mat4& M,
                             const std::map<Tree::Id, float>& vs)
    : root_op(root->op)
{
    setMatrix(M);

    auto flat = root.ordered();

    // Helper function to create a new clause in the data array
    // The dummy clause (0) is mapped to the first result slot
    std::unordered_map<Tree::Id, Clause::Id> clauses = {{nullptr, 0}};
    Clause::Id id = flat.size();

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
    std::map<Clause::Id, float> constants;
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
            constants[id] = vs.at(m.id());
            vars.left.insert({id, m.id()});
            var_handles.insert({m.id(), m});
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
    tapes.push_back(Tape());
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
    result.resize(clauses.size() + 1, vars.size());
    disabled.resize(clauses.size() + 1);
    remap.resize(clauses.size() + 1);

    // Store all constants in results array
    for (auto c : constants)
    {
        result.fill(c.second, c.first);
    }

    // Save X, Y, Z ids
    X = clauses.at(axes[0].id());
    Y = clauses.at(axes[1].id());
    Z = clauses.at(axes[2].id());

    // Set derivatives for X, Y, Z (unchanging)
    result.setDeriv(1, 0, 0, X);
    result.setDeriv(0, 1, 0, Y);
    result.setDeriv(0, 0, 1, Z);

    {   // Set the Jacobian for our variables (unchanging)
        size_t index = 0;
        for (auto v : vars.left)
        {
            result.setGradient(v.first, index++);
        }
    }

    // Store the index of the tree's root
    assert(clauses.at(root.id()) == 1);
    tape->i = clauses.at(root.id());
}

////////////////////////////////////////////////////////////////////////////////

float EvaluatorBase::eval(float x, float y, float z)
{
    set(x, y, z, 0);
    return values(1)[0];
}

Interval EvaluatorBase::eval(Interval x, Interval y, Interval z)
{
    set(x, y, z);
    return interval();
}

void EvaluatorBase::set(Interval x, Interval y, Interval z)
{
    result.i[X] = M[0][0] * x + M[1][0] * y + M[2][0] * z + M[3][0];
    result.i[Y] = M[0][1] * x + M[1][1] * y + M[2][1] * z + M[3][1];
    result.i[Z] = M[0][2] * x + M[1][2] * y + M[2][2] * z + M[3][2];
}

////////////////////////////////////////////////////////////////////////////////

void EvaluatorBase::pushTape()
{
    auto prev_tape = tape;

    // Add another tape to the top of the tape stack if one doesn't already
    // exist (we never erase them, to avoid re-allocating memory during
    // nested evaluations).
    if (++tape == tapes.end())
    {
        tape = tapes.insert(tape, Tape());
        tape->t.reserve(tapes.front().t.size());
    }
    else
    {
        // We may be reusing an existing tape, so resize to 0
        // (preserving allocated storage)
        tape->t.clear();
    }

    assert(tape != tapes.end());
    assert(tape != tapes.begin());
    assert(tape->t.capacity() >= prev_tape->t.size());

    // Now, use the data in disabled and remap to make the new tape
    for (const auto& c : prev_tape->t)
    {
        if (!disabled[c.id])
        {
            Clause::Id ra, rb;
            for (ra = c.a; remap[ra]; ra = remap[ra]);
            for (rb = c.b; remap[rb]; rb = remap[rb]);
            tape->t.push_back({c.op, c.id, ra, rb});
        }
    }

    // Remap the tape root index
    for (tape->i = prev_tape->i; remap[tape->i]; tape->i = remap[tape->i]);

    // Make sure that the tape got shorter
    assert(tape->t.size() <= prev_tape->t.size());
}

void EvaluatorBase::push()
{
    // Since we'll be figuring out which clauses are disabled and
    // which should be remapped, we reset those arrays here
    std::fill(disabled.begin(), disabled.end(), true);
    std::fill(remap.begin(), remap.end(), 0);

    // Mark the root node as active
    disabled[tape->i] = false;

    for (const auto& c : tape->t)
    {
        if (!disabled[c.id])
        {
            // For min and max operations, we may only need to keep one branch
            // active if it is decisively above or below the other branch.
            if (c.op == Opcode::MAX)
            {
                if (result.i[c.a].lower() > result.i[c.b].upper())
                {
                    disabled[c.a] = false;
                    remap[c.id] = c.a;
                }
                else if (result.i[c.b].lower() > result.i[c.a].upper())
                {
                    disabled[c.b] = false;
                    remap[c.id] = c.b;
                }
            }
            else if (c.op == Opcode::MIN)
            {
                if (result.i[c.a].lower() > result.i[c.b].upper())
                {
                    disabled[c.b] = false;
                    remap[c.id] = c.b;
                }
                else if (result.i[c.b].lower() > result.i[c.a].upper())
                {
                    disabled[c.a] = false;
                    remap[c.id] = c.a;
                }
            }
            if (!remap[c.id])
            {
                disabled[c.a] = false;
                disabled[c.b] = false;
            }
            else
            {
                disabled[c.id] = true;
            }
        }
    }

    pushTape();
}

Feature EvaluatorBase::push(const Feature& f)
{
    // Since we'll be figuring out which clauses are disabled and
    // which should be remapped, we reset those arrays here
    std::fill(disabled.begin(), disabled.end(), true);
    std::fill(remap.begin(), remap.end(), 0);

    // Mark the root node as active
    disabled[tape->i] = false;

    Feature out;
    out.deriv = f.deriv;

    const auto& choices = f.getChoices();
    auto itr = choices.begin();

    for (const auto& c : tape->t)
    {
        const bool match =  (result.f[c.a][0] == result.f[c.b][0] &&
                            (c.op == Opcode::MAX || c.op == Opcode::MIN) &&
                            itr != choices.end() && itr->id == c.id);

        if (!disabled[c.id])
        {
            // For ambiguous min and max operations, we obey the feature in
            // terms of which branch to take
            if (match)
            {
                out.push_raw(*itr, f.getEpsilon(c.id));

                if (itr->choice == 0)
                {
                    disabled[c.a] = false;
                    remap[c.id] = c.a;
                }
                else
                {
                    disabled[c.b] = false;
                    remap[c.id] = c.b;
                }
            }

            if (!remap[c.id])
            {
                disabled[c.a] = false;
                disabled[c.b] = false;
            }
            else
            {
                disabled[c.id] = true;
            }
        }

        if (match)
        {
            ++itr;
        }
    }
    assert(itr == choices.end());

    pushTape();
    return out;
}

void EvaluatorBase::specialize(float x, float y, float z)
{
    // Load results into the first floating-point result slot
    eval(x, y, z);

    // The same logic as push, but using float instead of interval comparisons
    std::fill(disabled.begin(), disabled.end(), true);
    std::fill(remap.begin(), remap.end(), 0);

    // Mark the root node as active
    disabled[tape->i] = false;

    for (const auto& c : tape->t)
    {
        if (!disabled[c.id])
        {
            // For min and max operations, we may only need to keep one branch
            // active if it is decisively above or below the other branch.
            if (c.op == Opcode::MAX)
            {
                if (result.f[c.a][0] > result.f[c.b][0])
                {
                    disabled[c.a] = false;
                    remap[c.id] = c.a;
                }
                else if (result.f[c.b][0] > result.f[c.a][0])
                {
                    disabled[c.b] = false;
                    remap[c.id] = c.b;
                }
            }
            else if (c.op == Opcode::MIN)
            {
                if (result.f[c.a][0] > result.f[c.b][0])
                {
                    disabled[c.b] = false;
                    remap[c.id] = c.b;
                }
                else if (result.f[c.b][0] > result.f[c.a][0])
                {
                    disabled[c.a] = false;
                    remap[c.id] = c.a;
                }
            }
            if (!remap[c.id])
            {
                disabled[c.a] = false;
                disabled[c.b] = false;
            }
            else
            {
                disabled[c.id] = true;
            }
        }
    }

    pushTape();
}

bool EvaluatorBase::isInside(float x, float y, float z)
{
    set(x, y, z, 0);
    auto vs = values(1);

    // Unambiguous cases
    if (vs[0] < 0)
    {
        return true;
    }
    else if (vs[0] > 0)
    {
        return false;
    }

    // Otherwise, we need to handle the zero-crossing case!

    // First, we extract all of the features
    auto fs = featuresAt(x, y, z);

    // If there's only a single feature, we can get both positive and negative
    // values out if it's got a non-zero gradient
    if (fs.size() == 1)
    {
        return fs.front().deriv.norm() > 0;
    }

    // Otherwise, check each feature
    // The only case where we're outside the model is if all features
    // and their normals are all positive (i.e. for every epsilon that
    // we move from (x,y,z), epsilon . deriv > 0)
    bool pos = false;
    bool neg = false;
    for (auto& f : fs)
    {
        pos |= f.isCompatible(f.deriv);
        neg |= f.isCompatible(-f.deriv);
    }
    return !(pos && !neg);
}

std::list<Feature> EvaluatorBase::featuresAt(float x, float y, float z)
{
    // The initial feature doesn't know any ambiguities
    Feature f;
    std::list<Feature> todo = {f};
    std::list<Feature> done;
    std::set<std::list<Feature::Choice>> seen;

    // Load the location into the first results slot and evaluate
    specialize(x, y, z);

    while (todo.size())
    {
        // Take the most recent feature and scan for ambiguous min/max nodes
        // (from the bottom up).  If we find such an ambiguous node, then push
        // both versions to the feature (if compatible) and re-insert the
        // augmented feature in the todo list; otherwise, move the feature
        // to the done list.
        auto f = todo.front();
        todo.pop_front();

        // Then, push into this feature
        // (storing a minimized version of the feature)
        auto f_ = push(f);

        // Run a single evaluation of the value + derivatives
        // The value will be the same, but derivatives may change
        // depending on which feature we've pushed ourselves into
        const auto ds = derivs(1);

        bool ambiguous = false;
        for (auto itr = tape->t.rbegin(); itr != tape->t.rend(); ++itr)
        {
            // Check for ambiguity here
            if ((itr->op == Opcode::MIN || itr->op == Opcode::MAX) &&
                    result.f[itr->a][0] == result.f[itr->b][0])
            {
                // Check both branches of the ambiguity
                const Eigen::Vector3d rhs(result.dx[itr->b][0],
                                          result.dy[itr->b][0],
                                          result.dz[itr->b][0]);
                const Eigen::Vector3d lhs(result.dx[itr->a][0],
                                          result.dy[itr->a][0],
                                          result.dz[itr->a][0]);
                const auto epsilon = (itr->op == Opcode::MIN) ? (rhs - lhs)
                                                              : (lhs - rhs);

                auto fa = f_;
                if (fa.push(epsilon, {itr->id, 0}))
                {
                    todo.push_back(fa);
                }

                auto fb = f_;
                if (fb.push(-epsilon, {itr->id, 1}))
                {
                    todo.push_back(fb);
                }
                ambiguous = true;
                break;
            }
        }

        if (!ambiguous)
        {
            f_.deriv = {ds.dx[0], ds.dy[0], ds.dz[0]};
            if (seen.find(f_.getChoices()) == seen.end())
            {
                seen.insert(f_.getChoices());
                done.push_back(f_);
            }
        }
        pop(); // push(Feature)
    }
    pop(); // specialization

    return done;
}

std::set<Result::Index> EvaluatorBase::getAmbiguous(Result::Index i) const
{
    std::set<Result::Index> out;
    for (const auto& c : tape->t)
    {
        if (c.op == Opcode::MIN || c.op == Opcode::MAX)
        {
            for (Result::Index j=0; j < i; ++j)
            {
                if (result.f[c.a][j] == result.f[c.b][j])
                {
                    out.insert(j);
                }
            }
        }
    }
    return out;
}

void EvaluatorBase::pop()
{
    assert(tape != tapes.begin());
    tape--;
}

////////////////////////////////////////////////////////////////////////////////

#define EVAL_LOOP for (Result::Index i=0; i < count; ++i)
void EvaluatorBase::eval_clause_values(Opcode::Opcode op,
        const float* __restrict a, const float* __restrict b,
        float* __restrict out, Result::Index count)
{
    switch (op) {
        case Opcode::ADD:
            EVAL_LOOP
            out[i] = a[i] + b[i];
            break;
        case Opcode::MUL:
            EVAL_LOOP
            out[i] = a[i] * b[i];
            break;
        case Opcode::MIN:
            EVAL_LOOP
            out[i] = fmin(a[i], b[i]);
            break;
        case Opcode::MAX:
            EVAL_LOOP
            out[i] = fmax(a[i], b[i]);
            break;
        case Opcode::SUB:
            EVAL_LOOP
            out[i] = a[i] - b[i];
            break;
        case Opcode::DIV:
            EVAL_LOOP
            out[i] = a[i] / b[i];
            break;
        case Opcode::ATAN2:
            EVAL_LOOP
            out[i] = atan2(a[i], b[i]);
            break;
        case Opcode::POW:
            EVAL_LOOP
            out[i] = pow(a[i], b[i]);
            break;
        case Opcode::NTH_ROOT:
            EVAL_LOOP
            out[i] = pow(a[i], 1.0f/b[i]);
            break;
        case Opcode::MOD:
            EVAL_LOOP
            {
                out[i] = std::fmod(a[i], b[i]);
                while (out[i] < 0)
                {
                    out[i] += b[i];
                }
            }
            break;
        case Opcode::NANFILL:
            EVAL_LOOP
            out[i] = std::isnan(a[i]) ? b[i] : a[i];
            break;

        case Opcode::SQUARE:
            EVAL_LOOP
            out[i] = a[i] * a[i];
            break;
        case Opcode::SQRT:
            EVAL_LOOP
            out[i] = sqrt(a[i]);
            break;
        case Opcode::NEG:
            EVAL_LOOP
            out[i] = -a[i];
            break;
        case Opcode::SIN:
            EVAL_LOOP
            out[i] = sin(a[i]);
            break;
        case Opcode::COS:
            EVAL_LOOP
            out[i] = cos(a[i]);
            break;
        case Opcode::TAN:
            EVAL_LOOP
            out[i] = tan(a[i]);
            break;
        case Opcode::ASIN:
            EVAL_LOOP
            out[i] = asin(a[i]);
            break;
        case Opcode::ACOS:
            EVAL_LOOP
            out[i] = acos(a[i]);
            break;
        case Opcode::ATAN:
            EVAL_LOOP
            out[i] = atan(a[i]);
            break;
        case Opcode::EXP:
            EVAL_LOOP
            out[i] = exp(a[i]);
            break;

        case Opcode::CONST_VAR:
            EVAL_LOOP
            out[i] = a[i];
            break;

        case Opcode::INVALID:
        case Opcode::CONST:
        case Opcode::VAR_X:
        case Opcode::VAR_Y:
        case Opcode::VAR_Z:
        case Opcode::VAR:
        case Opcode::LAST_OP: assert(false);
    }
}

void EvaluatorBase::eval_clause_derivs(Opcode::Opcode op,
        const float* __restrict av,  const float* __restrict adx,
        const float* __restrict ady, const float* __restrict adz,

        const float* __restrict bv,  const float* __restrict bdx,
        const float* __restrict bdy, const float* __restrict bdz,

        float* __restrict ov,  float* __restrict odx,
        float* __restrict ody, float* __restrict odz,
        Result::Index count)
{
    // Evaluate the base operations in a single pass
    eval_clause_values(op, av, bv, ov, count);

    switch (op) {
        case Opcode::ADD:
            EVAL_LOOP
            {
                odx[i] = adx[i] + bdx[i];
                ody[i] = ady[i] + bdy[i];
                odz[i] = adz[i] + bdz[i];
            }
            break;
        case Opcode::MUL:
            EVAL_LOOP
            {   // Product rule
                odx[i] = av[i]*bdx[i] + adx[i]*bv[i];
                ody[i] = av[i]*bdy[i] + ady[i]*bv[i];
                odz[i] = av[i]*bdz[i] + adz[i]*bv[i];
            }
            break;
        case Opcode::MIN:
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
        case Opcode::MAX:
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
        case Opcode::SUB:
            EVAL_LOOP
            {
                odx[i] = adx[i] - bdx[i];
                ody[i] = ady[i] - bdy[i];
                odz[i] = adz[i] - bdz[i];
            }
            break;
        case Opcode::DIV:
            EVAL_LOOP
            {
                const float p = pow(bv[i], 2);
                odx[i] = (bv[i]*adx[i] - av[i]*bdx[i]) / p;
                ody[i] = (bv[i]*ady[i] - av[i]*bdy[i]) / p;
                odz[i] = (bv[i]*adz[i] - av[i]*bdz[i]) / p;
            }
            break;
        case Opcode::ATAN2:
            EVAL_LOOP
            {
                const float d = pow(av[i], 2) + pow(bv[i], 2);
                odx[i] = (adx[i]*bv[i] - av[i]*bdx[i]) / d;
                ody[i] = (ady[i]*bv[i] - av[i]*bdy[i]) / d;
                odz[i] = (adz[i]*bv[i] - av[i]*bdz[i]) / d;
            }
            break;
        case Opcode::POW:
            EVAL_LOOP
            {
                const float m = pow(av[i], bv[i] - 1);

                // The full form of the derivative is
                // odx[i] = m * (bv[i] * adx[i] + av[i] * log(av[i]) * bdx[i]))
                // However, log(av[i]) is often NaN and bdx[i] is always zero,
                // (since it must be CONST), so we skip that part.
                odx[i] = m * (bv[i] * adx[i]);
                ody[i] = m * (bv[i] * ady[i]);
                odz[i] = m * (bv[i] * adz[i]);
            }
            break;
        case Opcode::NTH_ROOT:
            EVAL_LOOP
            {
                const float m = pow(av[i], 1.0f/bv[i] - 1);
                odx[i] = m * (1.0f/bv[i] * adx[i]);
                ody[i] = m * (1.0f/bv[i] * ady[i]);
                odz[i] = m * (1.0f/bv[i] * adz[i]);
            }
            break;
        case Opcode::MOD:
            EVAL_LOOP
            {
                // This isn't quite how partial derivatives of mod work,
                // but close enough normals rendering.
                odx[i] = adx[i];
                ody[i] = ady[i];
                odz[i] = adz[i];
            }
            break;
        case Opcode::NANFILL:
            EVAL_LOOP
            {
                odx[i] = std::isnan(av[i]) ? bdx[i] : adx[i];
                ody[i] = std::isnan(av[i]) ? bdy[i] : ady[i];
                odz[i] = std::isnan(av[i]) ? bdz[i] : adz[i];
            }
            break;

        case Opcode::SQUARE:
            EVAL_LOOP
            {
                odx[i] = 2 * av[i] * adx[i];
                ody[i] = 2 * av[i] * ady[i];
                odz[i] = 2 * av[i] * adz[i];
            }
            break;
        case Opcode::SQRT:
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
        case Opcode::NEG:
            EVAL_LOOP
            {
                odx[i] = -adx[i];
                ody[i] = -ady[i];
                odz[i] = -adz[i];
            }
            break;
        case Opcode::SIN:
            EVAL_LOOP
            {
                const float c = cos(av[i]);
                odx[i] = adx[i] * c;
                ody[i] = ady[i] * c;
                odz[i] = adz[i] * c;
            }
            break;
        case Opcode::COS:
            EVAL_LOOP
            {
                const float s = -sin(av[i]);
                odx[i] = adx[i] * s;
                ody[i] = ady[i] * s;
                odz[i] = adz[i] * s;
            }
            break;
        case Opcode::TAN:
            EVAL_LOOP
            {
                const float s = pow(1/cos(av[i]), 2);
                odx[i] = adx[i] * s;
                ody[i] = ady[i] * s;
                odz[i] = adz[i] * s;
            }
            break;
        case Opcode::ASIN:
            EVAL_LOOP
            {
                const float d = sqrt(1 - pow(av[i], 2));
                odx[i] = adx[i] / d;
                ody[i] = ady[i] / d;
                odz[i] = adz[i] / d;
            }
            break;
        case Opcode::ACOS:
            EVAL_LOOP
            {
                const float d = -sqrt(1 - pow(av[i], 2));
                odx[i] = adx[i] / d;
                ody[i] = ady[i] / d;
                odz[i] = adz[i] / d;
            }
            break;
        case Opcode::ATAN:
            EVAL_LOOP
            {
                const float d = pow(av[i], 2) + 1;
                odx[i] = adx[i] / d;
                ody[i] = ady[i] / d;
                odz[i] = adz[i] / d;
            }
            break;
        case Opcode::EXP:
            EVAL_LOOP
            {
                const float e = exp(av[i]);
                odx[i] = e * adx[i];
                ody[i] = e * ady[i];
                odz[i] = e * adz[i];
            }
            break;

        case Opcode::CONST_VAR:
            EVAL_LOOP
            {
                odx[i] = adx[i];
                ody[i] = ady[i];
                odz[i] = adz[i];
            }
            break;

        case Opcode::INVALID:
        case Opcode::CONST:
        case Opcode::VAR_X:
        case Opcode::VAR_Y:
        case Opcode::VAR_Z:
        case Opcode::VAR:
        case Opcode::LAST_OP: assert(false);
    }
}

#define JAC_LOOP for (auto a = aj.begin(), b = bj.begin(), o = oj.begin(); a != aj.end(); ++a, ++b, ++o)
float EvaluatorBase::eval_clause_jacobians(Opcode::Opcode op,
        const float av,  std::vector<float>& aj,
        const float bv,  std::vector<float>& bj,
        std::vector<float>& oj)
{
    // Evaluate the base operations in a single pass
    float out;
    eval_clause_values(op, &av, &bv, &out, 1);

    switch (op) {
        case Opcode::ADD:
            JAC_LOOP
            {
                (*o) = (*a) + (*b);
            }
            break;
        case Opcode::MUL:
            JAC_LOOP
            {   // Product rule
                (*o) = av * (*b) + bv * (*a);
            }
            break;
        case Opcode::MIN:
            JAC_LOOP
            {
                if (av < bv)
                {
                    (*o) = (*a);
                }
                else
                {
                    (*o) = (*b);
                }
            }
            break;
        case Opcode::MAX:
            JAC_LOOP
            {
                if (av < bv)
                {
                    (*o) = (*b);
                }
                else
                {
                    (*o) = (*a);
                }
            }
            break;
        case Opcode::SUB:
            JAC_LOOP
            {
                (*o) = (*a) - (*b);
            }
            break;
        case Opcode::DIV:
            JAC_LOOP
            {
                const float p = pow(bv, 2);
                (*o) = (bv*(*a) - av*(*b)) / p;
            }
            break;
        case Opcode::ATAN2:
            JAC_LOOP
            {
                const float d = pow(av, 2) + pow(bv, 2);
                (*o) = ((*a)*bv - av*(*b)) / d;
            }
            break;
        case Opcode::POW:
            JAC_LOOP
            {
                const float m = pow(av, bv - 1);

                // The full form of the derivative is
                // (*o) = m * (bv * (*a) + av * log(av) * (*b)))
                // However, log(av) is often NaN and (*b) is always zero,
                // (since it must be CONST), so we skip that part.
                (*o) = m * (bv * (*a));
            }
            break;
        case Opcode::NTH_ROOT:
            JAC_LOOP
            {
                const float m = pow(av, 1.0f/bv - 1);
                (*o) = m * (1.0f/bv * (*a));
            }
            break;
        case Opcode::MOD:
            JAC_LOOP
            {
                // This isn't quite how partial derivatives of mod work,
                // but close enough normals rendering.
                (*o) = (*a);
            }
            break;
        case Opcode::NANFILL:
            JAC_LOOP
            {
                (*o) = std::isnan(av) ? (*b) : (*a);
            }
            break;

        case Opcode::SQUARE:
            JAC_LOOP
            {
                (*o) = 2 * av * (*a);
            }
            break;
        case Opcode::SQRT:
            JAC_LOOP
            {
                if (av < 0)
                {
                    (*o) = 0;
                }
                else
                {
                    (*o) = (*a) / (2 * out);
                }
            }
            break;
        case Opcode::NEG:
            JAC_LOOP
            {
                (*o) = -(*a);
            }
            break;
        case Opcode::SIN:
            JAC_LOOP
            {
                const float c = cos(av);
                (*o) = (*a) * c;
            }
            break;
        case Opcode::COS:
            JAC_LOOP
            {
                const float s = -sin(av);
                (*o) = (*a) * s;
            }
            break;
        case Opcode::TAN:
            JAC_LOOP
            {
                const float s = pow(1/cos(av), 2);
                (*o) = (*a) * s;
            }
            break;
        case Opcode::ASIN:
            JAC_LOOP
            {
                const float d = sqrt(1 - pow(av, 2));
                (*o) = (*a) / d;
            }
            break;
        case Opcode::ACOS:
            JAC_LOOP
            {
                const float d = -sqrt(1 - pow(av, 2));
                (*o) = (*a) / d;
            }
            break;
        case Opcode::ATAN:
            JAC_LOOP
            {
                const float d = pow(av, 2) + 1;
                (*o) = (*a) / d;
            }
            break;
        case Opcode::EXP:
            JAC_LOOP
            {
                const float e = exp(av);
                (*o) = e * (*a);
            }
            break;

        case Opcode::CONST_VAR:
            JAC_LOOP
            {
                (*o) = 0;
            }
            break;

        case Opcode::INVALID:
        case Opcode::CONST:
        case Opcode::VAR_X:
        case Opcode::VAR_Y:
        case Opcode::VAR_Z:
        case Opcode::VAR:
        case Opcode::LAST_OP: assert(false);
    }

    return out;
}

Interval EvaluatorBase::eval_clause_interval(
        Opcode::Opcode op, const Interval& a, const Interval& b)
{
    switch (op) {
        case Opcode::ADD:
            return a + b;
        case Opcode::MUL:
            return a * b;
        case Opcode::MIN:
            return boost::numeric::min(a, b);
        case Opcode::MAX:
            return boost::numeric::max(a, b);
        case Opcode::SUB:
            return a - b;
        case Opcode::DIV:
            return a / b;
        case Opcode::ATAN2:
            return atan2(a, b);
        case Opcode::POW:
            return boost::numeric::pow(a, b.lower());
        case Opcode::NTH_ROOT:
            return boost::numeric::nth_root(a, b.lower());
        case Opcode::MOD:
            return Interval(0.0f, b.upper()); // YOLO
        case Opcode::NANFILL:
            return (std::isnan(a.lower()) || std::isnan(a.upper())) ? b : a;

        case Opcode::SQUARE:
            return boost::numeric::square(a);
        case Opcode::SQRT:
            return boost::numeric::sqrt(a);
        case Opcode::NEG:
            return -a;
        case Opcode::SIN:
            return boost::numeric::sin(a);
        case Opcode::COS:
            return boost::numeric::cos(a);
        case Opcode::TAN:
            return boost::numeric::tan(a);
        case Opcode::ASIN:
            return boost::numeric::asin(a);
        case Opcode::ACOS:
            return boost::numeric::acos(a);
        case Opcode::ATAN:
            return boost::numeric::atan(a);
        case Opcode::EXP:
            return boost::numeric::exp(a);

        case Opcode::CONST_VAR:
            return a;

        case Opcode::INVALID:
        case Opcode::CONST:
        case Opcode::VAR_X:
        case Opcode::VAR_Y:
        case Opcode::VAR_Z:
        case Opcode::VAR:
        case Opcode::LAST_OP: assert(false);
    }
    return Interval();
}

////////////////////////////////////////////////////////////////////////////////

const float* EvaluatorBase::values(Result::Index count)
{
    for (auto itr = tape->t.rbegin(); itr != tape->t.rend(); ++itr)
    {
        eval_clause_values(itr->op,
                &result.f[itr->a][0], &result.f[itr->b][0],
                &result.f[itr->id][0], count);
    }

    return &result.f[tape->i][0];
}

EvaluatorBase::Derivs EvaluatorBase::derivs(Result::Index count)
{
    for (auto itr = tape->t.rbegin(); itr != tape->t.rend(); ++itr)
    {
        eval_clause_derivs(itr->op,
               &result.f[itr->a][0], &result.dx[itr->a][0],
               &result.dy[itr->a][0], &result.dz[itr->a][0],

               &result.f[itr->b][0], &result.dx[itr->b][0],
               &result.dy[itr->b][0], &result.dz[itr->b][0],

               &result.f[itr->id][0], &result.dx[itr->id][0],
               &result.dy[itr->id][0], &result.dz[itr->id][0],
               count);
    }

    // Apply the inverse matrix transform to our normals
    const auto index = tape->i;
    auto o = Mi * glm::vec4(0,0,0,1);
    for (size_t i=0; i < count; ++i)
    {
        auto n = Mi * glm::vec4(result.dx[index][i],
                                result.dy[index][i],
                                result.dz[index][i], 1) - o;
        result.dx[index][i] = n.x;
        result.dy[index][i] = n.y;
        result.dz[index][i] = n.z;
    }

    return { &result.f[index][0],  &result.dx[index][0],
             &result.dy[index][0], &result.dz[index][0] };
}

std::map<Tree::Id, float> EvaluatorBase::gradient(float x, float y, float z)
{
    set(x, y, z, 0);

    for (auto itr = tape->t.rbegin(); itr != tape->t.rend(); ++itr)
    {
        float av = result.f[itr->a][0];
        float bv = result.f[itr->b][0];
        std::vector<float>& aj = result.j[itr->a];
        std::vector<float>& bj = result.j[itr->b];

        result.f[itr->id][0] = eval_clause_jacobians(
                itr->op, av, aj, bv, bj, result.j[itr->id]);
    }

    std::map<Tree::Id, float> out;
    {   // Unpack from flat array into map
        // (to allow correlating back to VARs in Tree)
        const auto ti = tape->i;
        size_t index = 0;
        for (auto v : vars.left)
        {
            out[v.second] = result.j[ti][index++];
        }
    }
    return out;
}

Interval EvaluatorBase::interval()
{
    for (auto itr = tape->t.rbegin(); itr != tape->t.rend(); ++itr)
    {
        Interval a = result.i[itr->a];
        Interval b = result.i[itr->b];

        result.i[itr->id] = eval_clause_interval(itr->op, a, b);
    }
    return result.i[tape->i];
}

////////////////////////////////////////////////////////////////////////////////

void EvaluatorBase::applyTransform(Result::Index count)
{
    for (size_t i=0; i < count; ++i)
    {
        float x = result.f[X][i];
        float y = result.f[Y][i];
        float z = result.f[Z][i];
        result.f[X][i] = M[0][0] * x + M[1][0] * y + M[2][0] * z + M[3][0];
        result.f[Y][i] = M[0][1] * x + M[1][1] * y + M[2][1] * z + M[3][1];
        result.f[Z][i] = M[0][2] * x + M[1][2] * y + M[2][2] * z + M[3][2];
    }
}

////////////////////////////////////////////////////////////////////////////////

double EvaluatorBase::utilization() const
{
    return tape->t.size() / double(tapes.front().t.size());
}

void EvaluatorBase::setMatrix(const glm::mat4& m)
{
    M = m;
    Mi = glm::inverse(m);
}

void EvaluatorBase::setVar(Tree::Id var, float value)
{
    auto r = vars.right.find(var);
    if (r != vars.right.end())
    {
        result.setValue(value, r->second);
    }
}

std::map<Tree::Id, float> EvaluatorBase::varValues() const
{
    std::map<Tree::Id, float> out;

    for (auto v : vars.left)
    {
        out[v.second] = result.f[v.first][0];
    }
    return out;
}

bool EvaluatorBase::updateVars(const std::map<Kernel::Tree::Id, float>& vars_)
{
    bool changed = false;
    for (const auto& v : vars.left)
    {
        auto val = vars_.at(v.second);
        if (val != result.f[v.first][0])
        {
            setVar(v.second, val);
            changed = true;
        }
    }
    return changed;
}

}   // namespace Kernel
