#include <array>

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
}

void Editor::onResult(QString result)
{
    setResult(true, result);
    clearError();
}

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

void Editor::setError(QPair<uint32_t, uint32_t> begin,
                      QPair<uint32_t, uint32_t> end)
{
    auto selections = clearError(false);

    QTextCursor c(script_doc);
    c.movePosition(QTextCursor::Down, QTextCursor::MoveAnchor, begin.first);
    c.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, begin.second);
    c.movePosition(QTextCursor::Down, QTextCursor::KeepAnchor, end.first - begin.first);
    c.movePosition(QTextCursor::StartOfLine, QTextCursor::KeepAnchor);
    c.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor, end.second);

    QTextEdit::ExtraSelection s;
    s.cursor = c;
    s.format = error_format;
    selections.append(s);

    script->setExtraSelections(selections);
}

void Editor::onError(QString result, QPair<uint32_t, uint32_t> start,
                                     QPair<uint32_t, uint32_t> end)
{
    setResult(false, result);
    setError(start, end);
}

void Editor::setResult(bool valid, QString result)
{
    QTextCharFormat fmt;
    fmt.setForeground(valid ? Color::green : Color::red);
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
        .arg(s.res);

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
        std::array<float, 7> out;
        for (unsigned i=0; i < out.size() && ok; ++i)
        {
            out[i] = match.captured(i + 1).toFloat(&ok);
        }
        if (ok)
        {
            Settings s({out[0], out[1], out[2]},
                       {out[3], out[4], out[5]}, out[6]);

            emit(settingsChanged(s));
        }
    }
}
