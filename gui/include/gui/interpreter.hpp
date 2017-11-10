/*
Studio: a simple GUI for the Ao CAD kernel
Copyright (C) 2017  Matt Keeter

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
#include <QMutex>
#include <QThread>
#include <QTimer>
#include <libguile.h>

#include "gui/editor.hpp"

struct Documentation;
class Shape;

/*
 *  _Interpreter is a child object that lives in a secondary thread
 *
 *  This means interpreting can happen asynchronously in the background,
 *  using signals and slots to sync with the main GUI thread.
 */
class _Interpreter : public QObject
{
    Q_OBJECT
signals:
    /*
     *  Emitted when a valid result should be shown in the GUI
     */
    void gotResult(QString result);

    /*
     *  Emitted when an error should be drawn in the GUI
     *  start and end are pairs of line, column
     */
    void gotError(QString error, QString stack, Editor::Range pos);

    /*
     *  Emitted to pass a set of keywords (space-delimited) to
     *  the syntax highlighter
     */
    void keywords(QString kws);

    /*
     *  Emitted to pass documentation to the main window
     *  (for use in Documentation panes)
     */
    void docs(Documentation* docs);

    /*
     *  Emitted to return a list of Shapes for the renderer
     */
    void gotShapes(QList<Shape*> s);

    /*
     *  Emitted to tell the editor about textual positions
     */
    void gotVars(QMap<Kernel::Tree::Id, Editor::Range> vs);

protected slots:
    /*  Initializes everything that Scheme needs */
    void init();

    /*  Loads the script from script_in and runs it */
    void eval();

protected:
    /*  No one should be constructing this class except the Interpreter */
    _Interpreter();

    /*  This is the worker thread which the _Interpreter runs in */
    QThread thread;

    /*  _script is locked by mutex.  It is used to deliver a new script
     *  to the worker thread, where it is loaded into script */
    QMutex mutex;
    QString _script;

    /*  Lots of miscellaneous Scheme objects, constructed once
     *  during init() so that we don't need to build them over
     *  and over again at runtime */
    SCM scm_eval_sandboxed;
    SCM scm_port_eof_p;
    SCM scm_valid_sym;
    SCM scm_syntax_error_sym;
    SCM scm_numerical_overflow_sym;

    SCM scm_syntax_error_fmt;
    SCM scm_numerical_overflow_fmt;
    SCM scm_other_error_fmt;
    SCM scm_result_fmt;

    friend class Interpreter;
};

////////////////////////////////////////////////////////////////////////////////

/*
 *  Interpreter is a wrapper object that lives in the main thread
 */
class Interpreter : public QObject
{
    Q_OBJECT
public:
    Interpreter();
    void start();

public slots:
    void onScriptChanged(QString s);

signals:
    /*
     *  Forwarded signals from _Interpreter
     */
    void gotResult(QString result);
    void gotError(QString error, QString stack, Editor::Range p);
    void keywords(QString kws);
    void docs(Documentation* docs);
    void gotShapes(QList<Shape*> s);
    void gotVars(QMap<Kernel::Tree::Id, Editor::Range> vs);

    /*
     *  Emitted when the interpreter starts evaluation
     *  (delayed slightly so that short computations don't jitter)
     */
    void busy();

protected:
    _Interpreter interpreter;

    QTimer eval_timer;
    QTimer busy_timer;
};
