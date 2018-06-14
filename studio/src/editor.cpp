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

#include <QVBoxLayout>

#include "studio/editor.hpp"
#include "studio/syntax.hpp"
#include "studio/color.hpp"

Editor::Editor(QWidget* parent, bool do_syntax)
    : QWidget(parent), script(new Script), script_doc(script->document()),
      syntax(do_syntax ? new Syntax(script_doc) : nullptr),
      err(new QPlainTextEdit), err_doc(err->document())
{
    error_format.setUnderlineColor(Color::red);
    error_format.setUnderlineStyle(QTextCharFormat::SingleUnderline);

    script->setLineWrapMode(QPlainTextEdit::NoWrap);
    err->setReadOnly(true);

    {   // Use Courier as our default font
        QFont font;
        font.setFamily("Courier");
        QFontMetrics fm(font);
        script->setTabStopWidth(fm.width("  "));
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

    // Do parenthesis highlighting when the cursor moves
    if (syntax)
    {
        connect(script, &QPlainTextEdit::cursorPositionChanged, syntax,
                [=](){ syntax->matchParens(
                    script, script->textCursor().position()); });
    }

    // Emit the script whenever text changes
    connect(script, &QPlainTextEdit::textChanged, this, &Editor::onScriptChanged);

    // Emit modificationChanged to keep window in sync
    connect(script_doc, &QTextDocument::modificationChanged,
            this, &Editor::modificationChanged);

    // Emit undo / redo signals to keep window's menu in sync
    connect(script_doc, &QTextDocument::undoAvailable,
            this, &Editor::undoAvailable);
    connect(script_doc, &QTextDocument::redoAvailable,
            this, &Editor::redoAvailable);

    auto layout = new QVBoxLayout;
    layout->addWidget(script);
    layout->addWidget(err);
    layout->setMargin(0);
    layout->setSpacing(2);
    setLayout(layout);

    spinner.setInterval(150);
    connect(&spinner, &QTimer::timeout, this, &Editor::onSpinner);
}

void Editor::onSpinner()
{
    const QString spin[4] = { "◐ ", "◓ ", "◑ ", "◒ " };
    static int i = 0;
    i = (i + 1) % 4;
    setResult(Color::blue, spin[i]);
}

void Editor::onResult(QString result)
{
    spinner.stop();
    setResult(Color::green , result);
    clearError();
}

void Editor::onError(QString result, QString stack, Range p)
{
    spinner.stop();
    setResult(Color::red, result + "\n\nStack trace:\n" + stack);
    setError(p);
}

void Editor::onBusy()
{
    spinner.start();
    onSpinner();
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

QList<QTextEdit::ExtraSelection> Editor::clearError(bool set)
{
    auto selections = script->extraSelections();
    for (auto itr = selections.begin(); itr != selections.end(); ++itr)
    {
        if (itr->format == error_format)
        {
            itr = --selections.erase(itr);
        }
    }

    if (set)
    {
        script->setExtraSelections(selections);
    }
    return selections;
}

void Editor::setError(Range p)
{
    auto selections = clearError(false);

    QTextCursor c(script_doc);
    c.movePosition(QTextCursor::Down, QTextCursor::MoveAnchor, p.start_row);
    c.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, p.start_col);
    c.movePosition(QTextCursor::Down, QTextCursor::KeepAnchor, p.end_row - p.start_row);
    c.movePosition(QTextCursor::StartOfLine, QTextCursor::KeepAnchor);
    c.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor, p.end_col);

    QTextEdit::ExtraSelection s;
    s.cursor = c;
    s.format = error_format;
    selections.append(s);

    script->setExtraSelections(selections);
}

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

void Editor::setScript(const QString& s)
{
    first_change = true;
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

void Editor::setKeywords(QString kws)
{
    if (syntax)
    {
        syntax->setKeywords(kws);
        script_doc->contentsChange(0, 0, script_doc->toPlainText().length());
    }
}

void Editor::onSettingsChanged(Settings s)
{
    const auto render_str = s.toString();

    QTextCursor c(script_doc);
    c.beginEditBlock();
    auto match = s.settings_regex.match(script_doc->toPlainText());
    if (!match.hasMatch())
    {
        c.insertText(render_str);
        c.insertText("\n");
    }
    else if (match.captured(0) != render_str)
    {
        c.setPosition(match.capturedStart());
        c.setPosition(match.capturedEnd(), QTextCursor::KeepAnchor);
        c.removeSelectedText();
        c.insertText(render_str);
    }
    c.endEditBlock();
}

void Editor::onScriptChanged()
{
    auto txt = script_doc->toPlainText();
    emit(scriptChanged(txt));

    bool okay = true;
    const auto s = Settings::fromString(txt, &okay);
    if (okay)
    {
        emit(settingsChanged(s, first_change));
    }
    first_change = false;
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

void Editor::setVarValues(QMap<Kernel::Tree::Id, float> vs)
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
    auto comp = [&](Kernel::Tree::Id a, Kernel::Tree::Id b){
        auto& pa = vars[a];
        auto& pb = vars[b];
        return (pa.start_row != pb.start_row) ? (pa.start_row < pb.start_row)
                                              : (pa.start_col < pb.start_col);
    };
    std::set<Kernel::Tree::Id, decltype(comp)> ordered(comp);
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
        if (pos.value().start_row == line)
        {
            pos.value().start_col += offset;
            pos.value().end_col += offset;
        }
        else
        {
            line = pos.value().start_row;
            offset = 0;
        }

        drag_cursor.movePosition(QTextCursor::Start);
        drag_cursor.movePosition(
                QTextCursor::Down, QTextCursor::MoveAnchor, pos.value().start_row);
        drag_cursor.movePosition(
                QTextCursor::Right, QTextCursor::MoveAnchor, pos.value().start_col);

        const auto length_before = pos.value().end_col - pos.value().start_col;
        drag_cursor.movePosition(
                QTextCursor::Right, QTextCursor::KeepAnchor, length_before);
        drag_cursor.removeSelectedText();

        QString str;
        str.setNum(v.value());
        drag_cursor.insertText(str);
        auto length_after = str.length();

        pos.value().end_col = pos.value().start_col + length_after;
        offset += length_after - length_before;
    }

    // Disable the script again (because this is only called when we're
    // doing a drag operation in the 3D viewport, and the script should
    // be locked).
    drag_cursor.endEditBlock();
    script->setEnabled(false);
}
