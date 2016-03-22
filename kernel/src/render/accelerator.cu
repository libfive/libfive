#include <cstdio>

#include "ao/kernel/render/accelerator.hpp"
#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/eval/clause.hpp"

__global__ void mykernel(void)
{
    // Nothing to do here
}

int run(void)
{
    mykernel<<<1,1>>>();
    printf("Hello World!\n");
    return 0;
}

Accelerator::Accelerator(Evaluator* e)
{
    for (auto c : {e->X, e->Y, e->Z})
    {
        allocate(c);
    }
    for (auto c : e->matrix)
    {
        allocate(c);
    }
    for (auto c : e->constants)
    {
        allocate(c);
    }
    for (auto row : e->rows)
    {
        for (auto c : row)
        {
            allocate(c);
        }
    }
}

void Accelerator::allocate(Clause* c)
{
    cudaMalloc((void**)&mem[c], N * sizeof(float));
}
