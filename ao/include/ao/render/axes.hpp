#pragma once

namespace Kernel {
namespace Axis {

    /*  Enumerator to refer to spatial axes  */
    enum Axis { X = 1, Y = 2, Z = 4 };

    /*  a, Q[a], R[a] is right-handed */
    inline constexpr Axis Q(Axis a)
    {
        return (a == Axis::X) ? Axis::Y :
               (a == Axis::Y) ? Axis::Z
                              : Axis::X;
    }
    inline constexpr Axis R(Axis a)
    {
        return (a == Axis::X) ? Axis::Z :
               (a == Axis::Y) ? Axis::X
                              : Axis::Y;
    }

}   // namespace Axis
}   // namespace Kernel
