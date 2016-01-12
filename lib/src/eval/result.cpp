#include "ao/eval/result.hpp"

void Result::fill(double v)
{
    for (size_t i=0; i < count<double>(); ++i)
    {
        set<double>(v, i);
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
        m[i/8] = _mm256_set_ps(d[i],   d[i+1], d[i+2], d[i+3],
                               d[i+4], d[i+5], d[i+6], d[i+7]);
    }
}

void Result::unpackAVX()
{
    for (size_t i=0; i < 32; ++i)
    {
        float f[8];

        __m128 low =  _mm256_extractf128_ps(m[i], 0);
        __m128 high = _mm256_extractf128_ps(m[i], 1);

        _mm_store_ps(f, low);
        _mm_store_ps(f + 4, high);

        for (size_t j=0; j < 8; ++j)
        {
            d[i*8 + j] = f[7 - j];
        }
    }
}
