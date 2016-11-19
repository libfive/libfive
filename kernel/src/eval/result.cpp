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
    _mf.resize(clauses);
    _mdx.resize(clauses);
    _mdy.resize(clauses);
    _mdz.resize(clauses);
    _i.resize(clauses);

    _f  = reinterpret_cast<float(*)[N]>(&_mf[0]);
    _dx = reinterpret_cast<float(*)[N]>(&_mdx[0]);
    _dy = reinterpret_cast<float(*)[N]>(&_mdy[0]);
    _dz = reinterpret_cast<float(*)[N]>(&_mdz[0]);
#else
    _f.resize(clauses);
    _dx.resize(clauses);
    _dy.resize(clauses);
    _dz.resize(clauses);
#endif
}

void Result::fill(float v, Clause::Id clause)
{
    for (unsigned i=0; i < N; ++i)
    {
        _f[clause][i] = v;
        _dx[clause][i] = 0;
        _dy[clause][i] = 0;
        _dz[clause][i] = 0;
    }

    _i[clause] = Interval(v, v);
}

void Result::deriv(float x, float y, float z, Clause::Id clause)
{
    for (size_t i=0; i < N; ++i)
    {
        _dx[clause][i] = x;
        _dy[clause][i] = y;
        _dz[clause][i] = z;
    }
}
