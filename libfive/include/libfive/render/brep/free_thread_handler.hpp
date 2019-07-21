/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

namespace libfive {

class FreeThreadHandler
{
public:
    /*
     *  This method is called by threads that have (probably temporarily)
     *  nothing to do, as part of a spinlock cycle.  When called, this method
     *  should not affect anything other than the FreeThreadHandler itself and
     *  the thread on which it is called (e.g. putting the thread to sleep if
     *  there is a better use for the resources), and all effects should end
     *  before returning (so the net effect of the call is nothing other than
     *  time spent and thread allocation).  Naturally, the implementation must
     *  be thread-safe.
     */
    virtual void offerWait() = 0;
};

}   // namespace libfive
