#include "ao/kernel/eval/result.hpp"

Result::Result()
{
#ifdef __AVX__
    f  = reinterpret_cast<float*>(mf);
    dx = reinterpret_cast<float*>(mdx);
    dy = reinterpret_cast<float*>(mdy);
    dz = reinterpret_cast<float*>(mdz);
#endif
}

void Result::set(Interval V)
{
    i = V;
}

void Result::fill(float v)
{
    for (unsigned i=0; i < 256; ++i)
    {
        f[i] = v;
        dx[i] = 0;
        dy[i] = 0;
        dz[i] = 0;
    }

    i = Interval(v, v);
}

void Result::deriv(float x, float y, float z)
{
    for (size_t i=0; i < 256; ++i)
    {
        dx[i] = x;
        dy[i] = y;
        dz[i] = z;
    }
}
