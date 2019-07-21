/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

namespace libfive {

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

}   // namespace libfive
