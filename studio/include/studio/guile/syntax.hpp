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
    void setKeywords(QStringList kws) override;

public slots:
    void onCursorMoved(QPlainTextEdit* text) override;

protected:
    QPoint findMatchedParen(int pos);

    int searchLeft(int pos);
    int searchRight(int pos);

    /*  Overrides highlightBlock in QSyntaxHighlighter */
    void highlightBlock(const QString& text) override;

    /*  Define states to keep track of multiline strings and comments. */
    enum State { BASE = -1, STRING = 1, COMMENT_BANG = 2, COMMENT_BAR = 3 };

    /*  Structure to use as a rule when highlighting    */
    struct Rule
    {
        Rule() {}
        Rule(QString r, QTextCharFormat f, State si=BASE, State so=BASE,
             int capture=0)
            : regex(QRegularExpression(r)), format(f), state_in(si), state_out(so),
              capture(capture)
        { /* Nothing to do here */ }

        QRegularExpression regex;
        QTextCharFormat format;
        State state_in, state_out;
        int capture;
    };
    QList<Rule> rules;
    QTextCharFormat parens_highlight;

    /* We use a special rule for language keywords, to avoid having one rule
     * per keyword, which would slow things down by a lot. */
    Rule m_keyword;
    QSet<QString> m_keywords;

    /*  Used as temporary storage when syntax highlighting is disabled */
    QTextDocument* doc=nullptr;
};

}   // namespace Guile
}   // namespace Studio
