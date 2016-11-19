/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include "ao/kernel/eval/result.hpp"

void Result::resize(Index clauses)
{
#ifdef __AVX__
    mf.resize(clauses);
    mdx.resize(clauses);
    mdy.resize(clauses);
    mdz.resize(clauses);
    i.resize(clauses);

    f  = reinterpret_cast<float(*)[N]>(&mf[0]);
    dx = reinterpret_cast<float(*)[N]>(&mdx[0]);
    dy = reinterpret_cast<float(*)[N]>(&mdy[0]);
    dz = reinterpret_cast<float(*)[N]>(&mdz[0]);
#else
    f.resize(clauses);
    dx.resize(clauses);
    dy.resize(clauses);
    dz.resize(clauses);
#endif
}

void Result::fill(float v, Clause::Id clause)
{
    for (unsigned i=0; i < N; ++i)
    {
        f[clause][i] = v;
        dx[clause][i] = 0;
        dy[clause][i] = 0;
        dz[clause][i] = 0;
    }

    i[clause] = Interval(v, v);
}

void Result::deriv(float x, float y, float z, Clause::Id clause)
{
    for (size_t i=0; i < N; ++i)
    {
        dx[clause][i] = x;
        dy[clause][i] = y;
        dz[clause][i] = z;
    }
}
