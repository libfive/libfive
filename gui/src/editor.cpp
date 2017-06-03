#include "gui/editor.hpp"
#include "gui/syntax.hpp"

Editor::Editor(QWidget* parent)
    : QTextEdit(parent)
{
    setAcceptRichText(false);

    {   // Use Courier as our default font
        QFont font;
        font.setFamily("Courier");
        QFontMetrics fm(font);
        setTabStopWidth(fm.width("    "));
        document()->setDefaultFont(font);
    }
    // Create and bind a syntax highlighter
    auto syntax = new Syntax(document(), {});

    // Do parenthesis highlighting when the cursor moves
    connect(this, &QTextEdit::cursorPositionChanged, syntax,
            [=](){ syntax->matchParens(this, this->textCursor().position()); });

    // Emit the script whenever text changes
    connect(this, &QTextEdit::textChanged, this,
            [=](){ this->scriptChanged(this->document()->toPlainText()); });
}
