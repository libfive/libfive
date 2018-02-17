/*
libfive: a CAD kernel for modeling with implicit functions
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

#include <boost/container/small_vector.hpp>
#include "libfive/render/brep/region.hpp"
#include "libfive/eval/interval.hpp"

namespace Kernel {

/* The oracle is an interface class for the use of externally defined oracles
 * as primitives in trees.
 */

class Oracle
{
public:
    /*  Both forms of getRange are used for interval optimization; as such, an
     *  overly large range will affect performance but not precision.
     */
    virtual Interval::I getRange(Region<2> region) const = 0;
    virtual Interval::I getRange(Region<3> region) const = 0;


    /*  Since the gradient may be discontinuous at the queried point, a vector
     *  is returned; in each entry of the vector, the first element of the pair
     *  represents the gradient in some area adjoining the passed point.  The
     *  second element of the pair represents a normalized epsilon indicating
     *  the domain of this gradient: for small scalar e and normalized vector
     *  n, the gradient at point + en is the one whose associated epsilon is
     *  closest to n.
     */
    virtual boost::container::small_vector<std::pair<Eigen::Vector3f, Eigen::Vector3f>, 1>
        getGradients(Eigen::Vector3f point) const = 0;


    virtual float getValue(Eigen::Vector3f point) const = 0;

    virtual ~Oracle() = default;
};



} //Namespace Kernel
