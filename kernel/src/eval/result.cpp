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

Result::Result()
{
#ifdef __AVX__
    f  = reinterpret_cast<float*>(mf);
    dx = reinterpret_cast<float*>(mdx);
    dy = reinterpret_cast<float*>(mdy);
    dz = reinterpret_cast<float*>(mdz);
#endif
}

void Result::set(Interval V)
{
    i = V;
}

void Result::fill(float v)
{
    for (unsigned i=0; i < N; ++i)
    {
        f[i] = v;
        dx[i] = 0;
        dy[i] = 0;
        dz[i] = 0;
    }

    i = Interval(v, v);
}

void Result::deriv(float x, float y, float z)
{
    for (size_t i=0; i < N; ++i)
    {
        dx[i] = x;
        dy[i] = y;
        dz[i] = z;
    }
}
