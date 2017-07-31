#include <QVBoxLayout>

#include "gui/editor.hpp"
#include "gui/syntax.hpp"
#include "gui/color.hpp"

Editor::Editor(QWidget* parent)
    : QWidget(parent), txt(new QTextEdit), err(new QPlainTextEdit)
{
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

    // Create and bind a syntax highlighter
    auto syntax = new Syntax(txt->document());

    // Do parenthesis highlighting when the cursor moves
    connect(txt, &QTextEdit::cursorPositionChanged, syntax,
            [=](){ syntax->matchParens(txt, txt->textCursor().position()); });

    // Emit the script whenever text changes
    connect(txt, &QTextEdit::textChanged, txt,
            [=](){ this->scriptChanged(txt->document()->toPlainText()); });

    // Bind keyword signals to syntax highlighter
    // (used to communicate asychronously from interpreter)
    connect(this, &Editor::keywords, syntax, &Syntax::setKeywords);

    auto layout = new QVBoxLayout;
    layout->addWidget(txt);
    layout->addWidget(err);
    layout->setMargin(0);
    layout->setSpacing(2);
    setLayout(layout);
}

void Editor::onResultChanged(bool valid, QString result)
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
