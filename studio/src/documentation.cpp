/*
Studio: a simple GUI for the libfive CAD kernel
Copyright (C) 2017  Matt Keeter

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
#include <iostream>

#include <QAbstractItemView>
#include <QCompleter>
#include <QEvent>
#include <QKeyEvent>
#include <QLabel>
#include <QPointer>
#include <QRegularExpression>
#include <QTextBrowser>
#include <QVBoxLayout>

#include "studio/documentation.hpp"

namespace Studio {

DocumentationPane::DocumentationPane(Documentation docs)
    : m_search(new QLineEdit)
{
    // Flatten documentation into a single-level map
    QMap<QString, QString> fs;   // shape name -> docstring
    QMap<QString, QString> tags; // shape name -> unique tag (used in search)
    QMap<QString, QString> mods; // shape name -> module name
    long max_name = 0;
    for (auto mod : docs.keys())
    {
        for (auto f : docs[mod].keys())
        {
            fs.insert(f, docs[mod][f]);
            tags.insert(f, QString("i%1").arg(tags.size()));
            mods.insert(f, mod);
            max_name = std::max((long)f.length(), max_name);
        }
    }

    // The first word of the docstring should be the function name, which we
    // use to detect aliases (where the name doesn't match)
    QRegularExpression first_keyword(R"(^([\w\-_!?\*]+))");

    // Unpack documentation into a text box
    auto txt = new QTextBrowser();
    for (auto f : fs.keys())
    {
        const auto doc = fs[f];

        // Add padding so that the module names all line up
        const auto padding = QString("&nbsp;")
            .repeated(max_name - f.length() + 4);

        txt->insertHtml(
                "<tt><a name=\"" + tags[f] +
                "\" href=\"#" + tags[f] + "\">" + f + "</a>" +
                padding +
                "<font color=\"silver\">" + mods[f] + "</font>" +
                "</tt><br>");

        const auto m = first_keyword.match(doc);
        if (m.hasMatch()) {
            const auto f_ = m.captured(0);
            if (f != f_)
            {
                txt->insertHtml("<tt>" + f + "</tt>: alias for ");
                if (fs.count(f_) != 1)
                {
                    std::cerr << "DocumentationPane: missing alias "
                              << f_.toStdString() << " for " << f.toStdString()
                              << std::endl;
                    txt->insertHtml("<tt>" + f_ + "</tt> (missing)\n");
                }
                else
                {
                    txt->insertHtml("<tt><a href=\"#" + tags[f_] + "\">" + f_ + "</a></tt><br>");
                }
                txt->insertPlainText("\n");
            } else {
                auto lines = doc.split("\n");
                if (lines.size() > 0)
                {
                    txt->insertHtml("<tt>" + lines[0] + "</tt><br>");
                }
                for (int i=1; i < lines.size(); ++i)
                {
                    txt->insertPlainText(lines[i] + "\n");
                }
                txt->insertPlainText("\n");
            }
        } else {
            txt->insertHtml("<tt>" + f + "</tt>: Bad docstring format<br><br>");
        }
    }

    {   // Erase the two trailing newlines
        auto cursor = QTextCursor(txt->document());
        cursor.movePosition(QTextCursor::End);
        cursor.deletePreviousChar();
        cursor.deletePreviousChar();
    }
    txt->setReadOnly(true);
    txt->scrollToAnchor("#i1");
    txt->installEventFilter(this);

    {
        int max_width = 0;
        QFontMetrics fm(txt->font());
        for (auto line : txt->toPlainText().split("\n"))
        {
            max_width = std::max(max_width, fm.horizontalAdvance(line));
        }
        txt->setMinimumWidth(max_width + 40);
    }

    // Build a search bar
    auto completer = new QCompleter(fs.keys(), this);

    completer->setCaseSensitivity(Qt::CaseInsensitive);
    m_search->setCompleter(completer);
    connect(completer, QOverload<const QString&>::of(&QCompleter::highlighted),
            txt, [=](const QString& str){
                if (tags.count(str))
                {
                    txt->scrollToAnchor(tags[str]);
                }
            });
    connect(m_search, &QLineEdit::textChanged, txt, [=](const QString& str){
                for (auto& t : tags.keys())
                {
                    if (t.startsWith(str))
                    {
                        txt->scrollToAnchor(tags[t]);
                        return;
                    }
                }
            });
    m_search->installEventFilter(this);

    auto layout = new QVBoxLayout();
    auto row = new QHBoxLayout;
    layout->addWidget(txt);
    row->addSpacing(5);
    row->addWidget(new QLabel("🔍  "));
    row->addSpacing(5);
    row->addWidget(m_search);
    layout->addItem(row);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);
    setLayout(layout);

    setWindowTitle("Shape reference");
#ifdef Q_OS_MAC
    setWindowFlags(Qt::Tool);
#else
    setWindowFlags(Qt::Tool | Qt::CustomizeWindowHint |
                   Qt::WindowTitleHint | Qt::WindowCloseButtonHint);
#endif
}

void DocumentationPane::show() {
    QWidget::show();
    m_search->setFocus();
    m_search->selectAll();
}

void DocumentationPane::closeEvent(QCloseEvent* event) {
    m_search->completer()->popup()->hide();
    QWidget::closeEvent(event);
}

bool DocumentationPane::eventFilter(QObject* object, QEvent* event)
{
    Q_UNUSED(object);

    if (event->type() == QEvent::KeyPress &&
        static_cast<QKeyEvent*>(event)->key() == Qt::Key_Escape)
    {
        hide();
        return true;
    }
    else
    {
        return false;
    }
}

}   // namespace Studio
