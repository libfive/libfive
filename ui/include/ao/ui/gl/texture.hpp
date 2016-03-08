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
#include "ao/ui/gl/core.hpp"
#include "ao/kernel/render/heightmap.hpp"

/*
 *  Stores a DepthImage as a 1-channel depth texture in tex
 *
 *  The image should already be scaled so that -1 is closest to the front
 *  of the screen and pixels to be discarded have a depth value of 1
 */
void toDepthTexture(const DepthImage& img, GLuint tex);

/*
 *  Stores a NormalImage as a 4-channel 8-bit texture in tex
 */
void toNormalTexture(const NormalImage& img, GLuint tex);
