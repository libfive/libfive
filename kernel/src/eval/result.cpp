#include "ao/kernel/eval/result.hpp"

void Result::set(float v, float x, float y, float z, size_t index)
{
    f[index] = v;
    dx[index] = x;
    dy[index] = y;
    dz[index] = z;
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

#ifdef __AVX__
    for (size_t i=0; i < count<__m256>(); ++i)
    {
        m[i] = _mm256_set1_ps(v);
    }
#endif
}

#ifdef __AVX__
void Result::packAVX()
{
    for (size_t i=0; i < 256; i += 8)
    {
        m[i/8] = _mm256_load_ps(&f[i]);
    }
}

void Result::unpackAVX()
{
    for (size_t i=0; i < 32; ++i)
    {
        __m128 low =  _mm256_extractf128_ps(m[i], 0);
        __m128 high = _mm256_extractf128_ps(m[i], 1);

        _mm_store_ps(f + i*8, low);
        _mm_store_ps(f + i*8 + 4, high);
    }
}
#endif
