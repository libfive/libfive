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

    inline constexpr int toIndex(Axis a)
    {
        return (a == Axis::X) ? 0 :
               (a == Axis::Y) ? 1 :
               (a == Axis::Z) ? 2 : -1;
    }
    inline constexpr Axis toAxis(int i)
    {
        return (i == 0) ? Axis::X :
               (i == 1) ? Axis::Y :
               (i == 2) ? Axis::Z :
               static_cast<Axis>(-1);
    }

}   // namespace Axis
}   // namespace Kernel
