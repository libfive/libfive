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

#include "studio/syntax.hpp"

namespace Studio {

void Syntax::highlightBlock(const QString& text)
{
    int offset = 0;
    int state = previousBlockState();

    auto data = new BlockData;

    while (offset <= text.length())
    {
        Rule rule;
        QRegularExpressionMatch match;

        // First, check for a match against a language keyword by finding
        // a keyword-shaped blob of text, then comparing it against the map.
        auto m = m_keyword.regex.match(text, offset);
        if (m.hasMatch()) {
            if (m_keywords.contains(m.captured(0))) {
                match = m;
                rule = m_keyword;
            }
        }

        // Iterate over every rule, picking out the first match
        for (auto r : rules)
        {
            if (r.state_in != state)
            {
                continue;
            }

            auto m = r.regex.match(text, offset);
            if (!m.hasMatch())
            {
                continue;
            }

            if (!match.hasMatch() ||
                m.capturedStart(r.capture) < match.capturedStart(rule.capture) ||
                (m.capturedStart(r.capture) <= match.capturedStart(rule.capture) &&
                 m.capturedEnd(r.capture) > match.capturedEnd(rule.capture)))
            {
                match = m;
                rule = r;
            }
        }

        if (!match.isValid() || !match.hasMatch())
        {
            offset++;
            continue;
        }

        const auto start = match.capturedStart(rule.capture);
        const auto length = match.capturedLength(rule.capture);

        // Otherwise we end up in an infinite loop
        assert(length > 0);
        setFormat(start, length, rule.format);
        offset = start + length;
        state = rule.state_out;

        for (int i=1; i <= match.lastCapturedIndex(); ++i)
        {
            data->data.push_back({match.capturedStart(i), match.captured(i)});
        }
    }

    setCurrentBlockState(state);
    setCurrentBlockUserData(data);
}

void Syntax::setKeywords(QStringList kws) {
    for (auto k: kws) {
        m_keywords.insert(k);
    }
}

}   // namespace Studio
