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
#include <QRegularExpression>
#include <QSyntaxHighlighter>
#include <QPlainTextEdit>

namespace Studio {

class Syntax : public QSyntaxHighlighter
{
    Q_OBJECT
public:
    Syntax(QTextDocument* doc);
    void setKeywords(QStringList kws);

    using MatchInfo = std::pair<int, QString>;
    class BlockData : public QTextBlockUserData {
    public:
        std::list<MatchInfo> data;
    };

public slots:
    virtual void onCursorMoved(QPlainTextEdit* text)=0;

protected:
    /*  Overrides highlightBlock in QSyntaxHighlighter */
    void highlightBlock(const QString& text) override;

    /*  Structure to use as a rule when highlighting    */
    struct Rule {
        Rule() {}
        Rule(QString r, QTextCharFormat f, int si=0, int so=0, int capture=0)
            : regex(QRegularExpression(r)), format(f),
              state_in(si), state_out(so), capture(capture)
        { /* Nothing to do here */ }

        QRegularExpression regex;
        QTextCharFormat format;

        /*  Flags for multi-line sections (e.g. long comments or strings) */
        int state_in, state_out;

        /*  Marks which regex clause is highlighted */
        int capture;
    };

    /*  General-purpose rules */
    QList<Rule> m_rules;

    /* We use a special rule for language keywords, to avoid having one rule
     * per keyword, which would slow things down by a lot. */
    Rule m_keywordRule;
    QSet<QString> m_keywords;
};


////////////////////////////////////////////////////////////////////////////////

}   // namespace Studio
