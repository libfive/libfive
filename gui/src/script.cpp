#include <QDebug>
#include <QKeyEvent>

#include "gui/script.hpp"

void Script::keyPressEvent(QKeyEvent* e)
{
    if (e->key() == Qt::Key_Tab)
    {
        auto c = textCursor();
        if (c.hasSelection())
        {
            auto a = c.selectionStart();
            auto b = c.selectionEnd();

            c.setPosition(a);
            c.beginEditBlock();
            while (true)
            {
                c.movePosition(QTextCursor::StartOfLine);
                c.insertText("  ");
                c.movePosition(QTextCursor::Down);

                if (c.position() == a || c.position() > b)
                {
                    break;
                }
                a = c.position();
            }
            c.endEditBlock();
        }
        else
        {
            insertPlainText("  ");
            e->accept();
        }
    }
    else if (e->key() == Qt::Key_Backtab)
    {
        auto c = textCursor();
        if (c.hasSelection())
        {
            auto a = c.selectionStart();
            auto b = c.selectionEnd();

            c.setPosition(a);
            c.beginEditBlock();
            while (true)
            {
                c.movePosition(QTextCursor::StartOfLine);
                for (unsigned i=0; i < 2; ++i)
                {
                    c.movePosition(QTextCursor::Right,
                                   QTextCursor::KeepAnchor);
                    if (c.selectedText() == " ")
                    {
                        c.removeSelectedText();
                    }
                }
                c.movePosition(QTextCursor::Down);

                if (c.position() == a || c.position() > b)
                {
                    break;
                }
                a = c.position();
            }
            c.endEditBlock();
        }
    }
    else
    {
        QTextEdit::keyPressEvent(e);
    }
}
