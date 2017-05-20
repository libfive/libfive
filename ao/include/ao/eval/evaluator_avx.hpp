#pragma once

#include "ao/eval/evaluator_base.hpp"

namespace Kernel {

class EvaluatorAVX : public EvaluatorBase
{
public:
    /*
     *  Construct an evaluator for the given tree
     */
    EvaluatorAVX(const Tree root, const glm::mat4& M=glm::mat4(),
                 const std::map<Tree::Id, float>& vars=std::map<Tree::Id, float>())
        : EvaluatorBase(root, M, vars) { /* Nothing to do here */ }
    EvaluatorAVX(const Tree root, const std::map<Tree::Id, float>& vars)
        : EvaluatorBase(root, vars) { /* Nothing to do here */ }

    /*
     *  Copy constructor
     */
    EvaluatorAVX(const EvaluatorAVX& other)
        : EvaluatorBase(other) { /* Nothing to do here */ }

    /*
     *  AVX-accelerated versions of existing Evaluator functions
     */
    void applyTransform(Result::Index count);
    const float* values(Result::Index count);
    Derivs derivs(Result::Index count);

protected:
    static void eval_clause_values(Opcode::Opcode op,
            const __m256* __restrict a, const __m256* __restrict b,
                  __m256* __restrict out, Result::Index count);

    static void eval_clause_derivs(Opcode::Opcode op,
        const __m256* __restrict av,  const __m256* __restrict adx,
        const __m256* __restrict ady, const __m256* __restrict adz,

        const __m256* __restrict bv,  const __m256* __restrict bdx,
        const __m256* __restrict bdy, const __m256* __restrict bdz,

        __m256* __restrict ov,  __m256* __restrict odx,
        __m256* __restrict ody, __m256* __restrict odz,
        Result::Index count);
};

}   // namespace Kernel
