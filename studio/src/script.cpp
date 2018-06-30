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
#include <QDebug>
#include <QKeyEvent>

#include "studio/script.hpp"

void Script::keyPressEvent(QKeyEvent* e)
{
    // By default, we'll accept the event here.
    // If it doesn't meet any of our conditions, then we'll set it as
    // ignored in the final else statement, then call the default key
    // press handler for QPlainTextEdit.
    e->setAccepted(true);

    auto c = textCursor();
    if (e->key() == Qt::Key_Tab)
    {
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
                c.insertText("  ");
                c.movePosition(QTextCursor::Down);

                if (c.position() == a || c.position() > b)
                {
                    break;
                }
                a = c.position();
            }
            c.endEditBlock();
        }
        // Otherwise, it does aligned 2-space indentation
        else
        {
            insertPlainText((c.columnNumber() & 1) ? " " : "  ");
        }
    }
    else if (e->key() == Qt::Key_Backtab && c.hasSelection())
    {
        auto a = c.selectionStart();
        auto b = c.selectionEnd();

        c.setPosition(a);
        c.beginEditBlock();
        while (true)
        {
            c.movePosition(QTextCursor::StartOfLine);
            for (unsigned i=0; i < 2; ++i)
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
    else if (e->key() == Qt::Key_Return && !c.hasSelection())
    {
        // Count the number of leading spaces on this line
        c.movePosition(QTextCursor::StartOfLine);
        auto prev_pos = c.position();
        c.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor);
        while (*c.selectedText().rbegin() == ' ' && c.position() > prev_pos)
        {
            prev_pos = c.position();
            c.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor);
        }

        // Figure out if we stopped searching due to a non-space character
        // or hitting the end of the document
        auto length = c.selectedText().length() - 1;
        length += (*c.selectedText().rbegin() == ' ');

        // Then match the number of leading spaces
        insertPlainText("\n" + QString(length, ' '));
    }
    // Backspace does 2-space aligned deletion of spaces, and deletes
    // a single character otherwise.
    else if (e->key() == Qt::Key_Backspace && !c.hasSelection())
    {
        bool was_space;
        {
            auto c_ = textCursor();
            c_.movePosition(QTextCursor::Left, QTextCursor::KeepAnchor);
            was_space = (c_.selectedText() == " ");
        }

        c.deletePreviousChar();
        if (was_space && (c.columnNumber() & 1) && c.columnNumber() != 0)
        {
            c.movePosition(QTextCursor::Left, QTextCursor::KeepAnchor);
            if (c.selectedText() == " ")
            {
                c.deletePreviousChar();
            }
        }
    }
    else
    {
        e->setAccepted(false);
        QPlainTextEdit::keyPressEvent(e);
    }
}
