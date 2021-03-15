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
#include <QTextDocument>
#include <QTextBlockUserData>
#include <cassert>

#include "studio/color.hpp"
#include "studio/python/syntax.hpp"

namespace Studio {
namespace Python {

Syntax::Syntax(QTextDocument* doc)
    : ::Studio::Syntax(doc)
{
    {   // Strings
        QTextCharFormat string_format;
        string_format.setForeground(Color::green);

        // Strings on a single line
        m_rules << Rule(R"("(?:\\.|[^"\\])*")", string_format);
        m_rules << Rule(R"('(?:\\.|[^'\\])*')", string_format);
    }

    {   // Numbers (float and integer)
        QTextCharFormat num_format;
        num_format.setForeground(Color::orange);

        m_rules << Rule(R"(\b(?:-|)\d+\.\d*e\d+)", num_format);
        m_rules << Rule(R"(\b(?:-|)\d+\.\d*)", num_format);
        m_rules << Rule(R"(\b(?:-|)\d+e\d+)", num_format);
        m_rules << Rule(R"(\b(?:-|)\d+\b)", num_format);
    }

    {   // Comments!
        QTextCharFormat comment_format;
        comment_format.setForeground(Color::base1);

        m_rules << Rule(R"(#.*)", comment_format);
    }

    // Special regex for keywords, to avoid having one rule for each
    QTextCharFormat kw_format;
    kw_format.setForeground(Color::blue);
    m_keywordRule = Rule(R"((?<=[^\w-]|^)[\w_!?\*]+(?=[^\\w-]|$))", kw_format);
}

void Syntax::onCursorMoved(QPlainTextEdit* text)
{
    (void)text;
}

}   // namespace Guile
}   // namespace Studio
