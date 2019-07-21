/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <vector>

namespace libfive {

template <unsigned N>
class ManifoldTables
{
public:
    /*  Checks whether the given bitfield represents a manifold
     *  subspace boundary.  See the implementation for more details
     *  on what that means, as well as examples. */
    static bool manifold(uint32_t b);

protected:
    ManifoldTables();

    /*  We use a vector<bool> because of the bit-packing optimization,
     *  since these tables are quite large (4 MB for the 3D case) */
    std::vector<bool> data;

    /*  This array records whether a particular slot is yet known
     *  (because we lazily build the array as we get asked for more
     *  and more slots).  This doubles our RAM usage, but 8 MB isn't
     *  much to ask in this day and age. */
    std::vector<bool> known;
};

}   // namespace libfive
