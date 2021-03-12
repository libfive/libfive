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
#include "studio/guile/syntax.hpp"

////////////////////////////////////////////////////////////////////////////////

namespace Studio {
namespace Guile {

int Syntax::searchLeft(int pos)
{
    int depth = 1;
    for(QTextBlock b = document()->findBlock(pos); b.isValid(); b=b.previous())
    {
        auto data = static_cast<BlockData*>(b.userData());
        if (!data)
        {
            continue;
        }
        const auto& parens = data->data;

        // Search left to find a right-parens match
        auto start = std::find_if(parens.rbegin(), parens.rend(),
                [=](const MatchInfo& val)
                { return val.first + b.position() < pos; });

        for (auto itr=start; itr != parens.rend(); ++itr)
        {
            depth += (itr->second == ")") - (itr->second == "(");
            if (depth == 0)
            {
                return itr->first + b.position();
            }
        }
    }
    return -1;
}

int Syntax::searchRight(int pos)
{
    int depth = 1;
    for(QTextBlock b = document()->findBlock(pos); b.isValid(); b=b.next())
    {
        auto data = static_cast<BlockData*>(b.userData());
        if (!data)
        {
            continue;
        }
        const auto& parens = data->data;

        // Search right to find a left-parens match
        auto start = std::find_if(parens.begin(), parens.end(),
                [=](const MatchInfo& val)
                { return val.first + b.position() > pos; });

        for (auto itr=start; itr != parens.end(); ++itr)
        {
            depth += (itr->second == "(") - (itr->second == ")");
            if (depth == 0)
            {
                return itr->first + b.position();
            }
        }
    }
    return -1;
}

QPoint Syntax::findMatchedParen(int pos)
{
    auto block = document()->findBlock(pos);
    assert(block.isValid());

    auto data = static_cast<BlockData*>(block.userData());
    if (!data)
    {
        return {-1, -1};
    }

    const auto& parens = data->data;
    auto found = std::find_if(parens.begin(), parens.end(),
            [=](const MatchInfo& val)
            { return val.first + block.position() == pos ||
                     val.first + block.position() == pos - 1; });

    if (found == parens.end())
    {
        return {-1, -1};
    }

    auto p = found->first + block.position();
    if (found->second == ")")
    {
        return {p, searchLeft(p)};
    }
    else if (found->second == "(")
    {
        return {p, searchRight(p)};
    }

    assert(false);
    return {-1, -1};
}

////////////////////////////////////////////////////////////////////////////////

Syntax::Syntax(QTextDocument* doc)
    : ::Studio::Syntax(doc)
{
    {   // Strings (single and multi-line)
        QTextCharFormat string_format;
        string_format.setForeground(Color::green);

        // Strings on a single line
        // (with clever regex for escaped chars)
        rules << Rule(R"("(?:\\.|[^"\\])*")", string_format, BASE, BASE);

        // Multi-line strings
        rules << Rule(R"("(?:\\.|[^"\\])*$)", string_format, BASE, STRING);
        rules << Rule(R"(^(?:\\.|[^"\\])*")", string_format, STRING, BASE);
        rules << Rule(R"((?:\\.|[^"\\])+)", string_format, STRING, STRING);
    }

    {   // Variables (float and integer)
        QTextCharFormat var_format;
        var_format.setForeground(Color::orange);

        rules << Rule(R"(\B\#(?:-|)\d+\.\d*e\d+)", var_format);
        rules << Rule(R"(\B\#(?:-|)\d+\.\d*)", var_format);
        rules << Rule(R"(\B\#(?:-|)\d+e\d+)", var_format);
        rules << Rule(R"(\B\#(?:-|)\d+\b)", var_format);
    }

    {   // Numbers (float and integer)
        QTextCharFormat num_format;
        num_format.setForeground(Color::orange);

        rules << Rule(R"(\b(?:-|)\d+\.\d*e\d+)", num_format);
        rules << Rule(R"(\b(?:-|)\d+\.\d*)", num_format);
        rules << Rule(R"(\b(?:-|)\d+e\d+)", num_format);
        rules << Rule(R"(\b(?:-|)\d+\b)", num_format);
    }

    {   // Comments!
        QTextCharFormat comment_format;
        comment_format.setForeground(Color::base1);

        rules << Rule(R"(\;.*)", comment_format);

        // Single-line block comments of the form #| ... |#
        rules << Rule(R"(#\|(?:\|(?!#)|[^|])*\|#)", comment_format, BASE, BASE);

        // Multi-line block comment of the form #| ... |#
        rules << Rule(R"(#\|(?:\|(?!#)|[^|])*$)", comment_format, BASE, COMMENT_BAR);
        rules << Rule(R"(^(?:\|(?!#)|[^|])*\|#)", comment_format, COMMENT_BAR, BASE);
        rules << Rule(R"((?:\|(?!#)|[^|])+)", comment_format, COMMENT_BAR, COMMENT_BAR);

        // Block comments with #! ... !#
        rules << Rule(R"(#!(?:!(?!#)|[^!])*!#)", comment_format, BASE, BASE);
        rules << Rule(R"(#!(?:!(?!#)|[^!])*$)", comment_format, BASE, COMMENT_BANG);
        rules << Rule(R"(^(?:!(?!#)|[^!])*!#)", comment_format, COMMENT_BANG, BASE);
        rules << Rule(R"((?:!(?!#)|[^!])+)", comment_format, COMMENT_BANG, COMMENT_BANG);
    }

    // Special regex to catch parentheses
    // The target capture group is 1
    rules << Rule(R"([^()]*(\(|\)))", QTextCharFormat(), BASE, BASE, 1);

    // Set format for matched parentheses
    parens_highlight.setBackground(Color::base2);

    // Special regex for keywords, to avoid having one rule for each
    QTextCharFormat kw_format;
    kw_format.setForeground(Color::blue);
    m_keyword = Rule(R"((?<=[^\w-]|^)[\w\-!?\*]+(?=[^\\w-]|$))", kw_format);
}

void Syntax::onCursorMoved(QPlainTextEdit* text)
{
    // Erase previous parens-matching selections, leaving other
    // extra selections intact (e.g. for error highlighting)
    auto selections = text->extraSelections();
    selections.erase(
        std::remove_if(selections.begin(), selections.end(),
            [=](auto itr) { return itr.format == parens_highlight; }),
        selections.end());

    auto pos = findMatchedParen(text->textCursor().position());
    if (pos.x() != -1 && pos.y() != -1)
    {
        for (auto p : {pos.x(), pos.y()})
        {
            QTextCursor c(document());
            c.setPosition(p);
            c.movePosition(QTextCursor::NextCharacter, QTextCursor::KeepAnchor);

            QTextEdit::ExtraSelection s;
            s.cursor = c;
            s.format = parens_highlight;
            selections.append(s);
        }
    }
    text->setExtraSelections(selections);
}

}   // namespace Guile
}   // namespace Studio
