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
#include "studio/syntax.hpp"

namespace Studio {
namespace Guile {

/* Guile Scheme syntax highlighter */
class Syntax : public ::Studio::Syntax
{
    Q_OBJECT
public:
    Syntax(QTextDocument* doc);

public slots:
    void onCursorMoved(QPlainTextEdit* text) override;

protected:
    QPoint findMatchedParen(int pos);

    int searchLeft(int pos);
    int searchRight(int pos);

    /*  Define states to keep track of multiline strings and comments. */
    enum State { BASE = -1, STRING = 1, COMMENT_BANG, COMMENT_BAR };

    QTextCharFormat parens_highlight;
};

}   // namespace Guile
}   // namespace Studio
