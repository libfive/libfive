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
#include <QEvent>

#include "studio/python/formatter.hpp"

namespace Studio {
namespace Python {

void Formatter::keyPressEvent(QPlainTextEdit* doc, QKeyEvent* e)
{
    // Mostly copied from guile/formatter.cpp.  We accept the event here,
    // then ignore it below if it doesn't match any special cases.
    e->accept();

    auto c = doc->textCursor();
    if (e->key() == Qt::Key_Tab) {
        // Tab does block-level indentation if we've selected text
        if (c.hasSelection())
        {
            auto a = c.selectionStart();
            auto b = c.selectionEnd();

            c.setPosition(a);
            c.beginEditBlock();
            while (true)
            {
                c.movePosition(QTextCursor::StartOfLine);
                c.insertText("    ");
                c.movePosition(QTextCursor::Down);

                if (c.position() == a || c.position() > b)
                {
                    break;
                }
                a = c.position();
            }
            c.endEditBlock();
        }
        // Otherwise, it does aligned 4-space indentation
        else
        {
            doc->insertPlainText(QString(4 - (c.columnNumber() & 3), ' '));
        }
    }
    // Backtab does block-level de-indentation
    else if (e->key() == Qt::Key_Backtab && c.hasSelection()) {
        auto a = c.selectionStart();
        auto b = c.selectionEnd();

        c.setPosition(a);
        c.beginEditBlock();
        while (true)
        {
            c.movePosition(QTextCursor::StartOfLine);
            for (unsigned i=0; i < 4; ++i)
            {
                c.movePosition(QTextCursor::Right,
                               QTextCursor::KeepAnchor);
                if (c.selectedText() == " ")
                {
                    c.removeSelectedText();
                }
            }
            c.movePosition(QTextCursor::Down);

            if (c.position() == a || c.position() > b)
            {
                break;
            }
            a = c.position();
        }
        c.endEditBlock();
    }
    // On Return, indent to match the previous line's leading whitespace
    else if (e->key() == Qt::Key_Return && !c.hasSelection()) {

        c.movePosition(QTextCursor::StartOfLine);
        c.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor);
        unsigned indentation=0;
        while (c.selectedText() == " ") {
            indentation++;
            c.clearSelection();
            c.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor);
        }
        doc->insertPlainText("\n" + QString(indentation, ' '));
    }
    // Backspace does 4-space aligned deletion of spaces, and deletes
    // a single character otherwise.
    else if (e->key() == Qt::Key_Backspace && !c.hasSelection()) {
        // Delete one character
        bool was_space;
        {
            auto c_ = doc->textCursor();
            c_.movePosition(QTextCursor::Left, QTextCursor::KeepAnchor);
            was_space = (c_.selectedText() == " ");
        }
        c.deletePreviousChar();

        // Delete up to three more spaces, to column-align indentation
        for (unsigned i=0; was_space && i < 3; ++i) {
            if ((c.columnNumber() & 3) && c.columnNumber() != 0) {
                c.movePosition(QTextCursor::Left, QTextCursor::KeepAnchor);
                if (c.selectedText() == " ") {
                    c.deletePreviousChar();
                } else {
                    was_space = false;
                }
            }
        }
    }
    else
    {
        e->ignore();
    }
}

}   // namespace Python
}   // namespace Studio
