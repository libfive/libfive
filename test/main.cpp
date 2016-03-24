#define CATCH_CONFIG_RUNNER
#include <catch/catch.hpp>

#ifdef USE_CUDA
#include "ao/kernel/eval/accelerator.hpp"
#endif

int main(int argc, char** argv)
{
#ifdef USE_CUDA
    Accelerator::warmup();
#endif

    return Catch::Session().run(argc, argv);
}

