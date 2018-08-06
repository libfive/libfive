/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

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

/*
 *  A virtual class, used by Oracles for pushing
 *  (in the same way that Evaluators use Tapes to evaluate a subset
 *  of their operations)
 */
class OracleContext
{
public:
    virtual ~OracleContext() { /* Nothing to do here */ }

    /*
     *  Checks whether any subsequent push operations on the parent
     *  oracle can return a smaller context.  For example, a math
     *  tree is terminal if there are no min / max nodes in it.
     */
    virtual bool isTerminal() { return false; }
};

}   // namespace Kernel
