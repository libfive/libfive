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
#pragma once

#include "studio/interpreter.hpp"

// Forward declaration of PyObject, because including Python.h wreaks havok
// with Qt headers due to dueling definitions of "slots"
typedef struct _object PyObject;

namespace Studio {
namespace Python {

class Interpreter: public ::Studio::Interpreter {
    Q_OBJECT
public:
    Interpreter();
    ~Interpreter();

    void init() override;
    QString defaultScript() override;

public slots:
    void eval(QString s) override;

protected:
    PyObject* m_runFunc=NULL;

};

}   // namespace Python
}   // maespace Studio
