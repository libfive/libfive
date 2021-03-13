/*
Studio: a simple GUI for the libfive CAD kernel
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
#include <array>
#include <set>
#include <cassert>

#include <QLabel>
#include <QPushButton>

#include "studio/editor.hpp"
#include "studio/script.hpp"
#include "studio/color.hpp"

#ifdef STUDIO_WITH_GUILE
#include "studio/guile/language.hpp"
#endif

#ifdef STUDIO_WITH_PYTHON
#include "studio/python/language.hpp"
#endif

namespace Studio {

Editor::Editor(QWidget* parent)
    : QWidget(parent), script(new Script), script_doc(script->document()),
      err(new QPlainTextEdit), err_doc(err->document()),
      layout(new QVBoxLayout)
{
    error_format.setUnderlineColor(Color::red);
    error_format.setUnderlineStyle(QTextCharFormat::SingleUnderline);

    script->setLineWrapMode(QPlainTextEdit::NoWrap);
    err->setReadOnly(true);

    {   // Use Inconsolata as our default font
        QFont font("Inconsolata", 14);
        QFontMetrics fm(font);
        script->setTabStopDistance(fm.horizontalAdvance("  "));
        script_doc->setDefaultFont(font);
        err_doc->setDefaultFont(font);
        err->setFixedHeight(fm.height());
    }

    // Set background and base font color
    QString style = QString(
        "    background-color: %1;"
        "    color: %2;"
        "}").arg(Color::base3.name())
            .arg(Color::base00.name());

    setStyleSheet("QPlainTextEdit { " + style);

    // Emit the script whenever text changes
    connect(script, &QPlainTextEdit::textChanged,
            &m_textChangedDebounce, QOverload<>::of(&QTimer::start));

    // Emit modificationChanged to keep window in sync
    connect(script_doc, &QTextDocument::modificationChanged,
            this, &Editor::modificationChanged);

    // Emit undo / redo signals to keep window's menu in sync
    connect(script_doc, &QTextDocument::undoAvailable,
            this, &Editor::undoAvailable);
    connect(script_doc, &QTextDocument::redoAvailable,
            this, &Editor::redoAvailable);

    layout->addWidget(script);
    layout->addWidget(err);
    layout->setMargin(0);
    layout->setSpacing(2);
    setLayout(layout);

    spinner.setInterval(150);
    connect(&spinner, &QTimer::timeout, this, &Editor::onSpinner);

    m_textChangedDebounce.setInterval(250);
    m_textChangedDebounce.setSingleShot(true);
    connect(&m_textChangedDebounce, &QTimer::timeout,
            this, &Editor::onTextChangedDebounce);

    m_interpreterBusyDebounce.setInterval(100);
    m_interpreterBusyDebounce.setSingleShot(true);
    connect(&m_interpreterBusyDebounce, &QTimer::timeout,
            &spinner, QOverload<>::of(&QTimer::start));

    setLanguage(defaultLanguage());
}

void Editor::loadDefaultScript() {
    setScript(m_language->defaultScript());
    setModified(false);
}

void Editor::onInterpreterBusy() {
    m_interpreterBusyDebounce.start();
}

void Editor::onSpinner()
{
    const QString spin[4] = { "◐ ", "◓ ", "◑ ", "◒ " };
    static int i = 0;
    i = (i + 1) % 4;
    setResult(Color::blue, spin[i]);
}

void Editor::onInterpreterDone(Result r)
{
    m_interpreterBusyDebounce.stop();
    spinner.stop();

    // Remove error selections from the script
    auto selections = script->extraSelections();
    selections.erase(
        std::remove_if(selections.begin(), selections.end(),
            [=](auto itr) { return itr.format == error_format; }),
        selections.end());

    if (r.okay) {
        setResult(Color::green, r.result);

        // Store the textual position of variables, for later editing
        vars = r.vars;

        emit(shapes(r.shapes));
    } else {
        setResult(Color::red, r.error.error + "\n\nStack trace:\n" + r.error.stack);

        // Add new selections for errors in the script doc
        QTextCursor c(script_doc);
        c.movePosition(QTextCursor::Down, QTextCursor::MoveAnchor, r.error.range.top());
        c.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, r.error.range.left());
        c.movePosition(QTextCursor::Down, QTextCursor::KeepAnchor, r.error.range.height());
        c.movePosition(QTextCursor::StartOfLine, QTextCursor::KeepAnchor);
        c.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor, r.error.range.right());

        QTextEdit::ExtraSelection s;
        s.cursor = c;
        s.format = error_format;
        selections.append(s);
    }

    // Set selections, which may include new errors if !r.okay
    script->setExtraSelections(selections);

    // Clear warnings from the UI
    for (auto& w : findChildren<QVBoxLayout*>()) {
        if (w != layout) {
            for (int i=0; w != layout && i < w->count(); ++i) {
                auto item = w->itemAt(i)->widget();
                if (item) {
                    item->deleteLater();
                }
            }
            layout->removeItem(w);
            w->deleteLater();
        }
    }
    // Insert new warnings into the UI
    if (!r.warnings.isEmpty()) {
        QStringList fixes;

        auto v = new QVBoxLayout();
        v->setMargin(10);
        for (auto& f : r.warnings) {
            v->addWidget(new QLabel(f.first, this));
            if (!f.second.isEmpty()) {
                fixes.push_back(f.second);
            }
        }

        if (fixes.size()) {
            auto button = new QPushButton("Fix All", this);
            connect(button, &QPushButton::pressed, this, [=](){
                QTextCursor c(script_doc);
                for (auto& f : fixes)
                {
                    c.movePosition(QTextCursor::Start);
                    c.insertText(f);
                }});
            v->addWidget(button, 0, Qt::AlignHCenter);
        }
        layout->addLayout(v);
    }

    // Announce the settings
    emit(settingsChanged(r.settings, first_change));
    first_change = false;
}

void Editor::undo()
{
    script_doc->undo();
}

void Editor::redo()
{
    script_doc->redo();
}

////////////////////////////////////////////////////////////////////////////////

void Editor::setResult(QColor color, QString result)
{
    QTextCharFormat fmt;
    fmt.setForeground(color);
    err->setCurrentCharFormat(fmt);
    err->setPlainText(result);
    int lines = err_doc->size().height() + 1;
    QFontMetrics fm(err_doc->defaultFont());
    err->setFixedHeight(std::min(this->height()/3, lines * fm.lineSpacing()));
}

void Editor::setScript(const QString& s, bool reload)
{
    first_change = !reload;
    script->setPlainText(s);
}

QString Editor::getScript() const
{
    return script->toPlainText();
}

void Editor::setModified(bool m)
{
    script_doc->setModified(m);
    script_doc->modificationChanged(m);
}

void Editor::onTextChangedDebounce()
{
    auto txt = script_doc->toPlainText();
    emit(scriptChanged(txt));
}

void Editor::onDragStart()
{
    script->setEnabled(false);
    drag_should_join = false;
}

void Editor::onDragEnd()
{
    script->setEnabled(true);
    script->setFocus();
}

void Editor::setVarValues(QMap<libfive::Tree::Id, float> vs)
{
    // Temporarily enable the script so that we can edit the variable value
    script->setEnabled(true);
    QTextCursor drag_cursor(script_doc);
    if (drag_should_join)
    {
        drag_cursor.joinPreviousEditBlock();
    }
    else
    {
        drag_cursor.beginEditBlock();

        // There's a Qt bug where if you have a multiline selection,
        // editing the text within that selection causes it to be
        // rendered strangely until the pane is resized.  To work around this,
        // we clear selections when beginning a drag.
        auto cursor = script->textCursor();
        if (cursor.hasSelection())
        {
            cursor.clearSelection();
            script->setTextCursor(cursor);
        }
    }
    drag_should_join = true;

    // Build an ordered set so that we can walk through variables
    // in sorted line / column order, making offsets as textual positions
    // shift due to earlier variables in the same line
    auto comp = [&](libfive::Tree::Id a, libfive::Tree::Id b){
        auto& pa = vars[a];
        auto& pb = vars[b];
        return (pa.top() != pb.top()) ? (pa.top() < pb.top())
                                      : (pa.left() < pb.left());
    };
    std::set<libfive::Tree::Id, decltype(comp)> ordered(comp);
    for (auto v=vs.begin(); v != vs.end(); ++v)
    {
        ordered.insert(v.key());
    }

    int line = -1;
    int offset = 0;
    for (auto t : ordered)
    {
        auto v = vs.find(t);
        assert(v != vs.end());

        // Apply an offset to compensate for other variables that may have
        // changed already in this line
        auto pos = vars.find(v.key());
        assert(pos != vars.end());
        if (pos.value().top() == line)
        {
            pos.value().translate({offset, 0});
        }
        else
        {
            line = pos.value().top();
            offset = 0;
        }

        drag_cursor.movePosition(QTextCursor::Start);
        drag_cursor.movePosition(
                QTextCursor::Down, QTextCursor::MoveAnchor, pos.value().top());
        drag_cursor.movePosition(
                QTextCursor::Right, QTextCursor::MoveAnchor, pos.value().left());

        const auto length_before = pos.value().right() - pos.value().left();
        drag_cursor.movePosition(
                QTextCursor::Right, QTextCursor::KeepAnchor, length_before);
        drag_cursor.removeSelectedText();

        QString str;
        str.setNum(v.value());
        drag_cursor.insertText(str);
        auto length_after = str.length();

        pos.value().setRight(pos.value().left() + length_after);
        offset += length_after - length_before;
    }

    // Disable the script again (because this is only called when we're
    // doing a drag operation in the 3D viewport, and the script should
    // be locked).
    drag_cursor.endEditBlock();
    script->setEnabled(false);
}

void Editor::onSyntaxReady() {
    script_doc->contentsChange(0, 0, script_doc->toPlainText().length());
}

Language::Type Editor::defaultLanguage() {
#if defined(STUDIO_WITH_GUILE)
    return Language::LANGUAGE_GUILE;
#elif defined(STUDIO_WITH_PYTHON)
    return Language::LANGUAGE_PYTHON;
#else
#error Must have at least one language defined
#endif
}

bool Editor::supportsLanguage(Language::Type t) {
    switch (t) {
        case Language::LANGUAGE_GUILE:
#ifdef STUDIO_WITH_GUILE
            return true;
#else
            return false;
#endif
        case Language::LANGUAGE_PYTHON:
#ifdef STUDIO_WITH_PYTHON
            return true;
#else
            return false;
#endif
    }
}

void Editor::setLanguage(Language::Type t) {
    const bool was_default = m_language &&
        script_doc->toPlainText() == m_language->defaultScript();

    switch (t) {
#ifdef STUDIO_WITH_GUILE
        case Language::LANGUAGE_GUILE:
            m_language.reset(Studio::Guile::language(script));
            break;
#endif
#ifdef STUDIO_WITH_PYTHON
        case Language::LANGUAGE_PYTHON:
            m_language.reset(Studio::Python::language(script));
            break;
#endif
        default: return;
    }

    connect(script, &QPlainTextEdit::cursorPositionChanged,
            m_language.data(), [&](){ m_language->onCursorMoved(script); });
    connect(m_language.data(), &Language::interpreterBusy,
            &m_interpreterBusyDebounce, QOverload<>::of(&QTimer::start));
    connect(m_language.data(), &Language::interpreterDone,
            this, &Editor::onInterpreterDone);
    connect(m_language.data(), &Language::syntaxReady,
            this, &Editor::onSyntaxReady);

    connect(this, &Editor::scriptChanged,
            m_language.data(), &Language::onScriptChanged);
    connect(this, &Editor::onShowDocs, m_language.data(), &Language::onShowDocs);

    if (was_default) {
        loadDefaultScript();
    }
}

}   // namespace Studio
