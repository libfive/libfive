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
#include "libfive/render/brep/neighbors.hpp"
#include "libfive/render/brep/xtree.hpp"

namespace Kernel {

// Here are all of our static variables
template <unsigned N> std::array<uint8_t, _pow(3, N)-1> Neighbors<N>::fixed;
template <unsigned N> std::array<uint8_t, _pow(3, N)-1> Neighbors<N>::floating;
template <unsigned N> bool Neighbors<N>::loaded = false;
template <unsigned N> std::array<uint8_t, 1 << (2 * N)> Neighbors<N>::remap;

// Explicit initialization of template
template class Neighbors<2>;
template class Neighbors<3>;

}   // namespace Kernel
