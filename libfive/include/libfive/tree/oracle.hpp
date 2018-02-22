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

/*The oracle is an interface class for the use of externally defined oracles 
 *as primitives in trees.
 */

class Oracle 
{
public:
    class GradientsWithEpsilons
    {
    public:
        enum PriorityType {USEFURTHEST, USECLOSEST};

        GradientsWithEpsilons(
            boost::container::small_vector<Eigen::Vector3d, 1> gradients,
            PriorityType priority);

        const boost::container::small_vector<
            std::pair<Eigen::Vector3d/*gradient*/,
            std::vector<Eigen::Vector3d>/*epsilons*/>,
            1>& getData() const { return data; }

        /*  Move-constructor equivalent; invalidates the object.  
         *  Use responsibly.
         */
        boost::container::small_vector<
            std::pair<Eigen::Vector3d/*gradient*/,
            std::vector<Eigen::Vector3d>/*epsilons*/>,
            1>&& moveData() { return std::move(data); }
    protected:
        /*  It is the derived class's responsibility to ensure that the data 
         *  fulfills the necessary invariants:
         *  1.  For any entry in data, there must be at least one vector that 
         *          has positive dot product with all epsilons for that entry;
         *          equivalently, all epsilons of a single entry must be 
         *          compatible.
         *  2.  Any nonzero vector must fulfill the above condition (the 
         *	        positive dot product) for exactly one gradient; that
         *          is, this condition must partition R3/{0} (or, equivalently, 
         *          S2) into regions that are disjoint except where they have
         *          the same gradient (in which case overlap is discouraged
         *          when easily avoidable but is permissible).  Said gradient
         *          should be the correct one to use at that point plus some
         *          sufficiently small positive multiple of any vector in that 
         *          region.
         *  3.  If two entries in data have epsilons such that their respective
         *          regions are adjacent, the difference between their 
         *          gradients must be a scalar multiple of the 
         *          epsilons producing the boundary between them.  (If they
         *          overlap, said difference must be 0 as per rule 2, and thus
         *          is a scalar multiple of any vectors.)
         */
        GradientsWithEpsilons(
            boost::container::small_vector<
            std::pair<Eigen::Vector3d, std::vector<Eigen::Vector3d>>, 1> data)
            : data(data) { ; }
    private:
        boost::container::small_vector<
            std::pair<Eigen::Vector3d/*gradient*/,
            std::vector<Eigen::Vector3d>/*epsilons*/>,
            1> data;
    };

    /*  Both forms of getRange are used for interval optimization; as such, an
     *  overly large range will affect performance but not precision.
     */
    virtual Interval::I getRange(Region<2> region, int threadNo) const = 0;
    virtual Interval::I getRange(Region<3> region, int threadNo) const = 0;

    virtual GradientsWithEpsilons
        getGradients(Eigen::Vector3f point, int threadNo) const = 0;

    /*  Should return true iff getGradients returns more than one gradient for
     *  the same point, but may optimize.
     */
    virtual bool isAmbiguous(Eigen::Vector3f point, int threadNo) const = 0;

    virtual float getValue(Eigen::Vector3f point, int threadNo) const = 0;

    virtual ~Oracle() = default;
};



} //Namespace Kernel