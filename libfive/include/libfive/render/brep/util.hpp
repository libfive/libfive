/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

/*  Compile-time power */
static constexpr unsigned ipow(unsigned p, unsigned n)
{ return (n == 0) ? 1 : p * ipow(p, n - 1); }

static constexpr unsigned bitcount(unsigned p)
{ return (p == 0) ? 0 : ((p & 1) + bitcount(p >> 1)); }
