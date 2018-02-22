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

#include "libfive/tree/oracle.hpp"

namespace Kernel {
Oracle::GradientsWithEpsilons::GradientsWithEpsilons(
    boost::container::small_vector<Eigen::Vector3d, 1> gradients,
    PriorityType priority)
    :data()
{
    for (auto& gradient1 : gradients)
    {
        std::pair<Eigen::Vector3d, std::vector<Eigen::Vector3d>>
            newEntry(gradient1, {});
        newEntry.second.reserve(gradients.size() - 1);
        auto duplicateGradient = false;
        for (auto& gradient2 : gradients)
        {
            if (gradient1 == gradient2) 
            {
                if (&gradient1 > &gradient2)
                {
                    duplicateGradient = true;
                    break;
                }
                else
                {
                    continue;
                }
            }
            else 
            {
                auto epsilon = (priority == USEFURTHEST)
                    ? gradient2 - gradient1
                    : gradient1 - gradient2;
                newEntry.second.push_back(epsilon);
            }
        }
        data.push_back(std::move(newEntry));
    }
}

} //namespace Kernel