#pragma once

#ifdef __AVX__
    #include "evaluator_avx.hpp"
    #define Evaluator EvaluatorAVX
#else
    #include "evaluator_base.hpp"
    #define Evaluator EvaluatorBase
#endif
