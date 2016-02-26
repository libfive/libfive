/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of Ao.
 *
 *  Ao is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <libguile.h>

#include "../api.hpp"

extern "C"
{
    void ao_init_guile();
}

static void* load(void* data)
{
    scm_c_primitive_load(static_cast<char*>(data));
    return nullptr;
}

static void callback(const char* filename)
{
    scm_with_guile(load, (void*)filename);
}

void ao_init_guile()
{
    window_watch_callback = callback;
}
