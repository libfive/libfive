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
                   Syntax* syntax)
    : m_interpreter(interpreter), m_formatter(formatter), m_syntax(syntax),
      m_showDocsWhenReady(false)
{
    // Connect to the interpreter thread
    connect(this, &Language::onScriptChanged,
            m_interpreter.data(), &Interpreter::eval);
    connect(m_interpreter.data(), &Interpreter::ready,
            this, &Language::onInterpreterReady);
    connect(m_interpreter.data(), &Interpreter::done,
            this, &Language::interpreterDone);
    connect(m_interpreter.data(), &Interpreter::busy,
            this, &Language::interpreterBusy);

    connect(&m_interpreterThread, &QThread::started,
            m_interpreter.data(), &Interpreter::init);
    m_interpreter->moveToThread(&m_interpreterThread);
    m_interpreterThread.start();
}

Language::~Language() {
    m_interpreterThread.quit();
    m_interpreterThread.wait();
}

void Language::onShowDocs() {
    if (m_docs) {
        m_docs->show();
    } else {
        m_showDocsWhenReady = true;
    }
}

QString Language::defaultScript() {
    return m_interpreter->defaultScript();
}

void Language::onInterpreterReady(QStringList keywords, Documentation docs) {
    m_syntax->setKeywords(keywords);
    m_docs.reset(new DocumentationPane(docs));
    if (m_showDocsWhenReady) {
        m_docs->show();
    }
}

}   // namespace Studio
