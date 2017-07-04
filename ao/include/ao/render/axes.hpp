#pragma once

namespace Kernel {
namespace Axis {

    /*  Enumerator to refer to spatial axes  */
    enum Axis { X = 1, Y = 2, Z = 4 };

    /*  a, Q[a], R[a] is right-handed */
    extern Axis Q[3];
    extern Axis R[3];

}   // namespace Axis
}   // namespace Kernel
