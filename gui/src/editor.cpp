#include <QVBoxLayout>

#include "gui/editor.hpp"
#include "gui/syntax.hpp"
#include "gui/color.hpp"

Editor::Editor(QWidget* parent)
    : QWidget(parent), txt(new QTextEdit), err(new QPlainTextEdit),
      syntax(new Syntax(txt->document()))
{
    error_format.setUnderlineColor(Color::red);
    error_format.setUnderlineStyle(QTextCharFormat::SingleUnderline);

    txt->setAcceptRichText(false);
    err->setReadOnly(true);

    {   // Use Courier as our default font
        QFont font;
        font.setFamily("Courier");
        QFontMetrics fm(font);
        txt->setTabStopWidth(fm.width("    "));
        txt->document()->setDefaultFont(font);
        err->document()->setDefaultFont(font);
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
    connect(txt, &QTextEdit::cursorPositionChanged, syntax,
            [=](){ syntax->matchParens(txt, txt->textCursor().position()); });

    // Emit the script whenever text changes
    connect(txt, &QTextEdit::textChanged, txt,
            [=](){ this->scriptChanged(txt->document()->toPlainText()); });

    auto layout = new QVBoxLayout;
    layout->addWidget(txt);
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
    auto selections = txt->extraSelections();
    for (auto itr = selections.begin(); itr != selections.end(); ++itr)
    {
        if (itr->format == error_format)
        {
            itr = --selections.erase(itr);
        }
    }

    if (set)
    {
        txt->setExtraSelections(selections);
    }
    return selections;
}

void Editor::setError(QPair<uint32_t, uint32_t> begin,
                      QPair<uint32_t, uint32_t> end)
{
    auto selections = clearError(false);

    QTextCursor c(txt->document());
    c.movePosition(QTextCursor::Down, QTextCursor::MoveAnchor, begin.first);
    c.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, begin.second);
    c.movePosition(QTextCursor::Down, QTextCursor::KeepAnchor, end.first - begin.first);
    c.movePosition(QTextCursor::StartOfLine, QTextCursor::KeepAnchor);
    c.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor, end.second);

    QTextEdit::ExtraSelection s;
    s.cursor = c;
    s.format = error_format;
    selections.append(s);

    txt->setExtraSelections(selections);
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
    int lines = err->document()->size().height() + 1;
    QFontMetrics fm(err->document()->defaultFont());
    err->setFixedHeight(std::min(this->height()/3, lines * fm.lineSpacing()));
}

void Editor::setScript(const QString& s)
{
    txt->setPlainText(s);
}

QString Editor::getScript() const
{
    return txt->toPlainText();
}

void Editor::setKeywords(QString kws)
{
    syntax->setKeywords(kws);
    auto doc = txt->document();
    doc->contentsChange(0, 0, doc->toPlainText().length());
}
