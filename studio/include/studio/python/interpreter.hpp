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
typedef struct _ts PyThreadState;

namespace Studio {
namespace Python {

class Interpreter: public ::Studio::Interpreter {
public:
    Interpreter();
    ~Interpreter();

    void init() override;
    QString defaultScript() override;
    QString extension() override { return ".py"; }

    void halt() override;
    void preinit() override;

public slots:
    void eval(QString s) override;

protected:
    // If the given PyObject represents a shape, record it in out.shapes
    void recordShape(
        PyObject* obj, Result &out, std::map<libfive::Tree::Id, float>& vars);

    PyObject* m_runFunc=NULL;
    PyObject* m_shapeClass=NULL;
    PyObject* m_varFunc=NULL;
    PyThreadState* m_threadState=NULL;
    unsigned long m_workerThreadId=0;

};

}   // namespace Python
}   // namespace Studio
