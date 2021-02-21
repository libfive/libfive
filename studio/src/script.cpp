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
        // Idea: having a smart indentation mechanism!
        // We search backwards until we find a opening parenthesis
        // that wasn't closed yet.
        // Now we check how many characters are infront of the opening parenthesis
        // and how many characters the first keyword takes.
        //
        // This should produce the following without typing any spaces/tabs
        // (+ 1 2
        //    3 4 (-
        //           4 5)
        //    6)
        //
        // number of spaces that should be used for indentation
        int indentation = 0;

        // Move left until finding a opening parenthesis
        int closedParenthesis = 0;
        while (c.movePosition(QTextCursor::PreviousCharacter, QTextCursor::KeepAnchor)) {
            if (*c.selectedText().begin() == ')') {
                closedParenthesis += 1;
            } else if (*c.selectedText().begin() == '(') {
                // Is this our parenthesis we are looking for?
                if (closedParenthesis == 0) {
                    auto curPos = c.position();

                    // Start of line
                    c.movePosition(QTextCursor::StartOfLine);
                    auto startPos = c.position();

                    c.setPosition(curPos);
                    // move cursor to the next white space
                    while (c.movePosition(QTextCursor::NextCharacter, QTextCursor::KeepAnchor))
                    {
                        if (c.selectedText().rbegin()->isSpace()) {
                            c.movePosition(QTextCursor::PreviousCharacter, QTextCursor::KeepAnchor);
                            break;
                        }
                    }
                    auto endPos = c.position();

                    indentation = endPos - startPos + 1;
                    break;

                } else {
                    closedParenthesis -= 1;
                }
            }
        }

        // insert indentation spaces
        insertPlainText("\n" + QString(indentation, ' '));
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
