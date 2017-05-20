#include "kernel/render/subregion.hpp"
#include "kernel/render/axes.hpp"

namespace Kernel
{

template <>
inline bool Subregion::canSplitEven<2>() const
{
    return X.size == Y.size &&
           X.size >= 2 && X.size % 2 == 0 &&
           Y.size >= 2 && Y.size % 2 == 0 &&
           Z.size == 1;
}

template <>
inline bool Subregion::canSplitEven<3>() const
{
    return X.size == Y.size && X.size == Z.size &&
           X.size >= 2 && X.size % 2 == 0 &&
           Y.size >= 2 && Y.size % 2 == 0 &&
           Z.size >= 2 && Z.size % 2 == 0;
}

////////////////////////////////////////////////////////////////////////////////

template <>
inline std::array<Subregion, 4> Subregion::splitEven<2>() const
{
    assert(canSplitEven<2>());

    auto x_ = X.split();
    auto y_ = Y.split();

    static_assert(AXIS_X == 1, "Axis layout is inconsistent");
    static_assert(AXIS_Y == 2, "Axis layout is inconsistent");

    return {{ Subregion(x_.first,  y_.first,  Z),
              Subregion(x_.second, y_.first,  Z),
              Subregion(x_.first,  y_.second, Z),
              Subregion(x_.second, y_.second, Z) }};
}

template <>
inline std::array<Subregion, 8> Subregion::splitEven<3>() const
{
    assert(canSplitEven<3>());

    auto x_ = X.split();
    auto y_ = Y.split();
    auto z_ = Z.split();

    static_assert(AXIS_X == 1, "Axis layout is inconsistent");
    static_assert(AXIS_Y == 2, "Axis layout is inconsistent");
    static_assert(AXIS_Z == 4, "Axis layout is inconsistent");

    return {{ Subregion(x_.first,  y_.first,  z_.first),
              Subregion(x_.second, y_.first,  z_.first),
              Subregion(x_.first,  y_.second, z_.first),
              Subregion(x_.second, y_.second, z_.first),
              Subregion(x_.first,  y_.first,  z_.second),
              Subregion(x_.second, y_.first,  z_.second),
              Subregion(x_.first,  y_.second, z_.second),
              Subregion(x_.second, y_.second, z_.second) }};
}

}   // namespace Kernel
