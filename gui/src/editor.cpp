#include <QVBoxLayout>

#include "gui/editor.hpp"
#include "gui/syntax.hpp"

Editor::Editor(QWidget* parent)
    : QWidget(parent)
{
    auto txt = new QTextEdit(this);
    txt->setAcceptRichText(false);

    auto err = new QPlainTextEdit(this);
    err->setEnabled(false);

    {   // Use Courier as our default font
        QFont font;
        font.setFamily("Courier");
        QFontMetrics fm(font);
        txt->setTabStopWidth(fm.width("    "));
        txt->document()->setDefaultFont(font);
        err->document()->setDefaultFont(font);
    }

    // Create and bind a syntax highlighter
    auto syntax = new Syntax(txt->document(), {});

    // Do parenthesis highlighting when the cursor moves
    connect(txt, &QTextEdit::cursorPositionChanged, syntax,
            [=](){ syntax->matchParens(txt, txt->textCursor().position()); });

    // Emit the script whenever text changes
    connect(txt, &QTextEdit::textChanged, txt,
            [=](){ this->scriptChanged(txt->document()->toPlainText()); });

    // Forward result changed into error window
    connect(this, &Editor::resultChanged, err,
            [=](bool valid, QString result){ err->setPlainText(result); });

    auto layout = new QVBoxLayout;
    layout->addWidget(txt);
    layout->addWidget(err);
    layout->setMargin(2);
    setLayout(layout);
}
