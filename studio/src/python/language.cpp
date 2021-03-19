/*
Studio: a simple GUI for the libfive CAD kernel
Copyright (C) 2021  Matt Keeter

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
#include "studio/script.hpp"

#include "studio/python/language.hpp"
#include "studio/python/interpreter.hpp"
#include "studio/python/formatter.hpp"
#include "studio/python/syntax.hpp"

namespace Studio {
namespace Python {

::Studio::Language* language(Script* script) {
    auto f = new Formatter();
    script->bind(f);
    return new Language(new Interpreter(), f, new Syntax(script->document()),
                        Language::LANGUAGE_PYTHON);
}

}   // namespace Python
}   // namespace Studio
