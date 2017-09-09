#include <array>
#include <set>
#include <cassert>

#include <QVBoxLayout>

#include "gui/editor.hpp"
#include "gui/syntax.hpp"
#include "gui/color.hpp"

Editor::Editor(QWidget* parent)
    : QWidget(parent), script(new QTextEdit), script_doc(script->document()),
      syntax(new Syntax(script_doc)), err(new QPlainTextEdit),
      err_doc(err->document())
{
    error_format.setUnderlineColor(Color::red);
    error_format.setUnderlineStyle(QTextCharFormat::SingleUnderline);

    script->setAcceptRichText(false);
    script->setLineWrapMode(QTextEdit::NoWrap);
    err->setReadOnly(true);

    {   // Use Courier as our default font
        QFont font;
        font.setFamily("Courier");
        QFontMetrics fm(font);
        script->setTabStopWidth(fm.width("    "));
        script_doc->setDefaultFont(font);
        err_doc->setDefaultFont(font);
        err->setFixedHeight(fm.height());
    }

    // Set background and base font color
    QString style = QString(
        "    background-color: %1;"
        "    color: %2;"
        "}").arg(Color::base3.name())
            .arg(Color::base00.name());

    setStyleSheet("QTextEdit {" + style + "QPlainTextEdit { " + style);

    // Do parenthesis highlighting when the cursor moves
    connect(script, &QTextEdit::cursorPositionChanged, syntax,
            [=](){ syntax->matchParens(script, script->textCursor().position()); });

    // Emit the script whenever text changes
    connect(script, &QTextEdit::textChanged, this, &Editor::onScriptChanged);

    // Emit modificationChanged to keep window in sync
    connect(script_doc, &QTextDocument::modificationChanged,
            this, &Editor::modificationChanged);

    auto layout = new QVBoxLayout;
    layout->addWidget(script);
    layout->addWidget(err);
    layout->setMargin(0);
    layout->setSpacing(2);
    setLayout(layout);

    spinner.setInterval(150);
    connect(&spinner, &QTimer::timeout, this, &Editor::onSpinner);
}

void Editor::onSpinner()
{
    const QString spin[4] = { "◐ ", "◓ ", "◑ ", "◒ " };
    static int i = 0;
    i = (i + 1) % 4;
    setResult(Color::blue, spin[i]);
}

void Editor::onResult(QString result)
{
    spinner.stop();
    setResult(Color::green , result);
    clearError();
}

void Editor::onError(QString result, QString stack, Range p)
{
    spinner.stop();
    setResult(Color::red, result + "\n\nStack trace:\n" + stack);
    setError(p);
}

void Editor::onBusy()
{
    spinner.start();
    onSpinner();
}

////////////////////////////////////////////////////////////////////////////////

QList<QTextEdit::ExtraSelection> Editor::clearError(bool set)
{
    auto selections = script->extraSelections();
    for (auto itr = selections.begin(); itr != selections.end(); ++itr)
    {
        if (itr->format == error_format)
        {
            itr = --selections.erase(itr);
        }
    }

    if (set)
    {
        script->setExtraSelections(selections);
    }
    return selections;
}

void Editor::setError(Range p)
{
    auto selections = clearError(false);

    QTextCursor c(script_doc);
    c.movePosition(QTextCursor::Down, QTextCursor::MoveAnchor, p.start_row);
    c.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, p.start_col);
    c.movePosition(QTextCursor::Down, QTextCursor::KeepAnchor, p.end_row - p.start_row);
    c.movePosition(QTextCursor::StartOfLine, QTextCursor::KeepAnchor);
    c.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor, p.end_col);

    QTextEdit::ExtraSelection s;
    s.cursor = c;
    s.format = error_format;
    selections.append(s);

    script->setExtraSelections(selections);
}

void Editor::setResult(QColor color, QString result)
{
    QTextCharFormat fmt;
    fmt.setForeground(color);
    err->setCurrentCharFormat(fmt);
    err->setPlainText(result);
    int lines = err_doc->size().height() + 1;
    QFontMetrics fm(err_doc->defaultFont());
    err->setFixedHeight(std::min(this->height()/3, lines * fm.lineSpacing()));
}

void Editor::setScript(const QString& s)
{
    script->setPlainText(s);
}

QString Editor::getScript() const
{
    return script->toPlainText();
}

void Editor::setModified(bool m)
{
    script_doc->setModified(m);
    script_doc->modificationChanged(m);
}

void Editor::setKeywords(QString kws)
{
    syntax->setKeywords(kws);
    script_doc->contentsChange(0, 0, script_doc->toPlainText().length());
}

void Editor::onSettingsChanged(Settings s)
{
    auto txt = script_doc->toPlainText();

    auto render_str = s.settings_fmt
        .arg(s.min.x()).arg(s.min.y()).arg(s.min.z())
        .arg(s.max.x()).arg(s.max.y()).arg(s.max.z())
        .arg(s.res).arg(s.quality);

    QTextCursor c(script_doc);

    c.beginEditBlock();
    auto match = s.settings_regex.match(txt);
    if (!match.hasMatch())
    {
        c.insertText(render_str);
        c.insertText("\n");
    }
    else if (match.captured(0) != render_str)
    {
        c.setPosition(match.capturedStart());
        c.setPosition(match.capturedEnd(), QTextCursor::KeepAnchor);
        c.removeSelectedText();
        c.insertText(render_str);
    }
    c.endEditBlock();
}

void Editor::onScriptChanged()
{
    auto txt = script_doc->toPlainText();
    emit(scriptChanged(txt));

    auto match = Settings::settings_regex.match(txt);
    if (match.hasMatch())
    {
        bool ok = true;
        std::vector<float> out;
        for (int i=1; i <= match.lastCapturedIndex() && ok; ++i)
        {
            out.push_back(match.captured(i).toFloat(&ok));
        }
        if (ok)
        {
            Settings s({out[0], out[1], out[2]},
                       {out[3], out[4], out[5]}, out[6], out[7]);

            emit(settingsChanged(s));
        }
    }
}

void Editor::setVarValues(QMap<Kernel::Tree::Id, float> vs)
{
    QTextCursor c(script_doc);

    // Build an ordered set so that we can walk through variables
    // in sorted line / column order, making offsets as textual positions
    // shift due to earlier variables in the same line
    auto comp = [&](Kernel::Tree::Id a, Kernel::Tree::Id b){
        auto& pa = vars[a];
        auto& pb = vars[b];
        return (pa.start_row != pb.start_row) ? (pa.start_row < pb.start_row)
                                              : (pa.start_col < pb.start_col);
    };
    std::set<Kernel::Tree::Id, decltype(comp)> ordered(comp);
    for (auto v=vs.begin(); v != vs.end(); ++v)
    {
        ordered.insert(v.key());
    }

    c.beginEditBlock();
    int line = -1;
    int offset = 0;
    for (auto t : ordered)
    {
        auto v = vs.find(t);
        assert(v != vs.end());

        // Apply an offset to compensate for other variables that may have
        // changed already in this line
        auto pos = vars.find(v.key());
        assert(pos != vars.end());
        if (pos.value().start_row == line)
        {
            pos.value().start_col += offset;
            pos.value().end_col += offset;
        }
        else
        {
            line = pos.value().start_row;
            offset = 0;
        }

        c.movePosition(QTextCursor::Start);
        c.movePosition(
                QTextCursor::Down, QTextCursor::MoveAnchor, pos.value().start_row);
        c.movePosition(
                QTextCursor::Right, QTextCursor::MoveAnchor, pos.value().start_col);

        const auto length_before = pos.value().end_col - pos.value().start_col;
        c.movePosition(
                QTextCursor::Right, QTextCursor::KeepAnchor, length_before);
        c.removeSelectedText();

        QString str;
        str.setNum(v.value());
        c.insertText(str);
        auto length_after = str.length();

        pos.value().end_col = pos.value().start_col + length_after;
        offset += length_after - length_before;
    }
    c.endEditBlock();
}
