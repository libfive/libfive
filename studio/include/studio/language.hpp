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
#include <QThread>
#include <QSyntaxHighlighter>

#include "studio/formatter.hpp"
#include "studio/interpreter.hpp"
#include "studio/result.hpp"
#include "studio/syntax.hpp"

namespace Studio {

/*
 *  This is a container class that abstracts over different languages.
 */
class Language : public QObject {
    Q_OBJECT
public:
    Language(Interpreter* interpreter,
             Formatter* formatter,
             Syntax* syntax);
    ~Language();

    QString defaultScript();

signals:
    /*  Emits the result of an interpreter evaluation */
    void interpreterDone(Result);
    void interpreterBusy();

    /*
     *  Called when the script changes.  This should be debounced in the Editor,
     *  because invoking it here will trigger immediate evaluation; it forwards
     *  the given string across the thread boundary to the interpreter.
     */
    void onScriptChanged(QString s);

public slots:
    void onShowDocs();

protected slots:
    /*
     *  Called when the interpreter finishes initialization, to populate the
     *  keywords and documentation panes.
     */
    void onInterpreterReady(QStringList keywords, Documentation docs);

protected:
    /*  A language must define three helper classes */
    QScopedPointer<Interpreter> m_interpreter;
    QScopedPointer<Formatter> m_formatter;
    QScopedPointer<Syntax> m_syntax;

    /*  This is the worker thread which the Interpreter runs in */
    QThread m_interpreterThread;

    /*  Constructed after the interpreter declares that it is ready */
    QScopedPointer<DocumentationPane> m_docs;

    /*  If the UI requests that docs be shown before the interpreter is
     *  booted, then we set this flag to show them once they're ready. */
    bool m_showDocsWhenReady;
};

} // namespace Studio
