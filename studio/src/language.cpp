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
#include "studio/language.hpp"

namespace Studio {

Language::Language(Interpreter* interpreter,
                   Formatter* formatter,
                   QSyntaxHighlighter* syntax)
    : m_interpreter(interpreter), m_formatter(formatter), m_syntax(syntax)
{
    connect(&m_interpreterThread, &QThread::started,
            m_interpreter.data(), &Interpreter::init);
    m_interpreter->moveToThread(&m_interpreterThread);
    m_interpreterThread.start();
}

QString Language::defaultScript() {
    return m_interpreter->defaultScript();
}

}   // namespace Studio
