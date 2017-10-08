/*
Ao: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
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
