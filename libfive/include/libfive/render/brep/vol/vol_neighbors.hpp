/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/render/brep/neighbors.hpp"

namespace libfive {

/*  Forward declaration */
class VolTree;

/*  This class isn't used for anything, but it's necessary for WorkerPool */
class VolNeighbors : public Neighbors<3, VolTree, VolNeighbors>
{
public:
    /*  Constructor, returning an empty neighbor array */
    VolNeighbors();
};

}   // namespace libfive
