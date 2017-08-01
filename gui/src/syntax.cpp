#include <QTextDocument>
#include <cassert>

#include "gui/syntax.hpp"
#include "gui/color.hpp"

////////////////////////////////////////////////////////////////////////////////

typedef std::pair<int, QString> MatchInfo;

class BlockData : public QTextBlockUserData
{
public:
    std::list<MatchInfo> data;
};

////////////////////////////////////////////////////////////////////////////////

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

QPoint Syntax::matchedParen(int pos)
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
    : QSyntaxHighlighter(doc)
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
        rules << Rule(R"(.+)", string_format, STRING, STRING);
    }

    {   // Numbers (float and integer)
        {   // All the possible float formats
            QTextCharFormat float_format;
            float_format.setForeground(Color::orange);

            rules << Rule(R"(\b(?:-|)\d+\.\d*e\d+)", float_format);
            rules << Rule(R"(\b(?:-|)\d+\.\d*)", float_format);
            rules << Rule(R"(\b(?:-|)\d+e\d+)", float_format);
        }

        {   // Integers
            QTextCharFormat int_format;
            int_format.setForeground(Color::red);

            rules << Rule(R"(\b(?:-|)\d+\b)", int_format);
        }
    }

    {   // Comments!
        QTextCharFormat comment_format;
        comment_format.setForeground(Color::base1);

        rules << Rule(R"(\;.*)", comment_format);
    }

    // Special regex to catch parentheses
    // The target capture group is 1
    rules << Rule(R"([^()]*(\(|\)))", QTextCharFormat(), BASE, BASE, 1);

    // Set format for matched parentheses
    parens_highlight.setBackground(Color::base2);
}

void Syntax::setKeywords(QString kws)
{
    QTextCharFormat kw_format;
    kw_format.setForeground(Color::blue);

    for (auto k : kws.split(' ', QString::SkipEmptyParts))
    {
        auto esc = QRegularExpression::escape(k);
        rules << Rule("\\b" + esc + "\\b", kw_format);
    }
}

void Syntax::matchParens(QTextEdit* text, int cursor_pos)
{
    // Erase previous parens-matching selections, leaving other
    // extra selections intact (e.g. for error highlighting)
    auto selections = text->extraSelections();
    for (auto itr = selections.begin(); itr != selections.end(); ++itr)
    {
        if (itr->format == parens_highlight)
        {
            itr = --selections.erase(itr);
        }
    }

    auto pos = matchedParen(cursor_pos);
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

void Syntax::highlightBlock(const QString& text)
{
    int offset = 0;
    int state = previousBlockState();

    auto data = new BlockData;

    while (offset <= text.length())
    {
        Rule rule;
        QRegularExpressionMatch match;

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
