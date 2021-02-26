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
        // Smart indentation, roughly matching the style described at
        //  http://community.schemewiki.org/?scheme-style
        //
        // We search backwards until we find a opening parenthesis
        // that wasn't closed yet, then search forward for a space, a newline
        // or an opening parenthesis, handling each case slightly differently.
        //
        // (with bonus special-casing for things like (lambda (x y z)...,
        //  which should only be indented by two characters on the next line)
        //
        // This should produce the following without typing any spaces/tabs
        // (+ 1 2
        //    3 4 (-
        //          4 5)
        //    6)
        //  (let ((i 10)
        //        (j 20))
        //    (+ i j))
        //  (long-function-name
        //    1 2 3)
        //  (long-function-name 1
        //                      2 3)
        //
        int indentation = 0;

        int closedParenthesis = 0;
        auto prev = [&c]() -> bool {
            return c.movePosition(QTextCursor::PreviousCharacter,
                                  QTextCursor::KeepAnchor);
        };
        auto next = [&c]() -> bool {
            return c.movePosition(QTextCursor::NextCharacter,
                                  QTextCursor::KeepAnchor);
        };

        // Move left until we find an unmatched opening parenthesis
        while (prev()) {
            if (*c.selectedText().begin() == ')') {
                closedParenthesis += 1;
            } else if (*c.selectedText().begin() == '(') {
                // Is this our parenthesis we are looking for?
                if (closedParenthesis == 0) {
                    // Move cursor to the next white space or open parenthesis
                    // If we don't find either before a newline, then offset
                    // stays at -1, which is handled differently.
                    int offset = -1;
                    const auto parenPos = c.position();
                    while (next())
                    {
                        const auto r = c.selectedText().begin();
                        if (*r == QChar(uint(0x2029))) { // Newline
                            break;
                        } else if (r->isSpace()) {
                            offset = 1;
                            break;
                        } else if (*r == '(') {
                            offset = 0;
                            break;
                        }
                    }
                    const auto endPos = c.position();
                    c.setPosition(endPos); // Clear the anchor
                    c.setPosition(parenPos + 1, QTextCursor::KeepAnchor);

                    // Special-case words which should have special behavior,
                    // e.g. "(define (x y z)" should only indent by two spaces
                    for (const auto s: {"let", "define", "lambda",
                                        "lambda-shape", "define-shape"}) {
                        if (c.selectedText() == s) {
                            offset = -1;
                            break;
                        }
                    }

                    // Find the start of the line
                    c.movePosition(QTextCursor::StartOfLine);
                    const auto startPos = c.position();

                    // If we found a good estimate of indentation within this
                    // line, then use it; otherwise, indent by two spaces.
                    if (offset != -1) {
                        indentation = endPos - startPos + offset;
                    } else {
                        indentation = parenPos - startPos + 2;
                    }
                    // Escape out of the indentation search loop
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
