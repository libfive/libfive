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

////////////////////////////////////////////////////////////////////////////////

typedef std::pair<int, QString> MatchInfo;

class BlockData : public QTextBlockUserData
{
public:
    std::list<MatchInfo> data;
};

////////////////////////////////////////////////////////////////////////////////

namespace Studio {
namespace Python {

////////////////////////////////////////////////////////////////////////////////

Syntax::Syntax(QTextDocument* doc)
    : ::Studio::Syntax(doc)
{
    // Nothing to do here
}

void Syntax::onCursorMoved(QPlainTextEdit* text)
{
    (void)text;
}

void Syntax::highlightBlock(const QString& text)
{
    // Nothing to do here
}

void Syntax::setKeywords(QStringList kws) {
    (void)kws;
}

}   // namespace Guile
}   // namespace Studio
