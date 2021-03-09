/*
Studio: a simple GUI for the libfive CAD kernel
Copyright (C) 2017-2021  Matt Keeter

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

#include <QObject>
#include <QTimer>

#include "studio/result.hpp"
#include "studio/documentation.hpp"

namespace Studio {

class Interpreter : public QObject
{
    Q_OBJECT
public:
    virtual ~Interpreter() { /* Nothing to do here */ }

    /*  Starts up the interpreter */
    virtual void init()=0;

    /*  Returns a reasonable default script */
    virtual QString defaultScript()=0;

public slots:
    /*  Evaluates a new script, returning done(...) when done. */
    virtual void eval(QString s)=0;

signals:
    void ready(QString keywords, Documentation docs);
    void busy();
    void done(Result);
};

} // namespace Studio
