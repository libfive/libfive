#include "ao/eval/result.hpp"

void Result::fill(float v)
{
    for (size_t i=0; i < count<float>(); ++i)
    {
        set<float>(v, i);
    }
    for (size_t i=0; i < count<Gradient>(); ++i)
    {
        set<Gradient>(Gradient(v), i);
    }
    for (size_t i=0; i < count<Interval>(); ++i)
    {
        set<Interval>(Interval(v), i);
    }
    for (size_t i=0; i < count<__m256>(); ++i)
    {
        m[i] = _mm256_set1_ps(v);
    }
}

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
