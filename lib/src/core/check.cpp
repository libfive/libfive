#include "ao/core/result.hpp"

////////////////////////////////////////////////////////////////////////////////
/*  Compile-time checking of object sizes  */
#define CHECK(condition) ((void)sizeof(char[1 - 2*!!(condition)]))
void CHECK_SIZES()
{
    CHECK(RESULT_ARRAY_BYTES % sizeof(double));
    CHECK(RESULT_ARRAY_BYTES % sizeof(Interval));
    CHECK(RESULT_ARRAY_BYTES % sizeof(Gradient));
    CHECK(sizeof(Interval) == sizeof(double));
    CHECK(sizeof(Interval) == sizeof(Gradient));
}
