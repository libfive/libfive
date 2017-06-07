#include <QVBoxLayout>

#include "gui/editor.hpp"
#include "gui/syntax.hpp"
#include "gui/material.hpp"

Editor::Editor(QWidget* parent)
    : QWidget(parent)
{
    auto txt = new QTextEdit(this);
    txt->setAcceptRichText(false);

    auto err = new QPlainTextEdit(this);
    err->setReadOnly(true);

    {   // Use Courier as our default font
        QFont font;
        font.setFamily("Courier");
        QFontMetrics fm(font);
        txt->setTabStopWidth(fm.width("    "));
        txt->document()->setDefaultFont(font);
        err->document()->setDefaultFont(font);
    }

    // Create and bind a syntax highlighter
    auto syntax = new Syntax(txt->document());

    // Do parenthesis highlighting when the cursor moves
    connect(txt, &QTextEdit::cursorPositionChanged, syntax,
            [=](){ syntax->matchParens(txt, txt->textCursor().position()); });

    // Emit the script whenever text changes
    connect(txt, &QTextEdit::textChanged, txt,
            [=](){ this->scriptChanged(txt->document()->toPlainText()); });

    // Forward result changed into error window
    connect(this, &Editor::resultChanged, err,
        [=](bool valid, QString result){
            QTextCharFormat fmt;
            fmt.setForeground(valid ? Material::green_800 : Material::red_800);
            err->setCurrentCharFormat(fmt);
            err->setPlainText(result);
            int lines = err->document()->size().height() + 1;
            QFontMetrics fm(err->document()->defaultFont());
            err->setFixedHeight(std::min(this->height()/3, lines * fm.lineSpacing()));
    });

    auto layout = new QVBoxLayout;
    layout->addWidget(txt);
    layout->addWidget(err);
    layout->setMargin(0);
    layout->setSpacing(2);
    setLayout(layout);
}
