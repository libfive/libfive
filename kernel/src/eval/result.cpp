#include "ao/kernel/eval/result.hpp"

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

#ifdef __AVX__
    for (size_t i=0; i < 32; ++i)
    {
        mf[i] = _mm256_set1_ps(v);
    }
#endif
}

void Result::deriv(float x, float y, float z)
{
    for (size_t i=0; i < 256; ++i)
    {
        dx[i] = x;
        dy[i] = y;
        dz[i] = z;
    }

#ifdef __AVX__
    for (size_t i=0; i < 32; ++i)
    {
        mdx[i] = _mm256_set1_ps(x);
        mdy[i] = _mm256_set1_ps(y);
        mdz[i] = _mm256_set1_ps(z);
    }
#endif
}

////////////////////////////////////////////////////////////////////////////////

#ifdef __AVX__
void Result::packAVX()
{
    for (size_t i=0; i < 256; i += 8)
    {
        mf[i/8] = _mm256_load_ps(&f[i]);
    }
}

void Result::unpackAVX()
{
    for (size_t i=0; i < 32; ++i)
    {
        __m128 low =  _mm256_extractf128_ps(mf[i], 0);
        __m128 high = _mm256_extractf128_ps(mf[i], 1);

        _mm_store_ps(f + i*8, low);
        _mm_store_ps(f + i*8 + 4, high);
    }
}
#endif
