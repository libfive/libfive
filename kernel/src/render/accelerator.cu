#include <cstdio>

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
