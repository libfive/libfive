#include <iostream>

#include <QTextEdit>
#include <QVBoxLayout>

#include "gui/documentation.hpp"

void Documentation::insert(QString module, QString name, QString doc)
{
    docs[module][name].doc = doc;
}

////////////////////////////////////////////////////////////////////////////////

QScopedPointer<Documentation> DocumentationPane::docs;

DocumentationPane::DocumentationPane()
{
    Q_ASSERT(docs.data());

    auto layout = new QVBoxLayout();

    auto txt = new QTextEdit();

    txt->setFontFamily("Courier");

    // Flatten documentation into a single-level map
    QMap<QString, QString> fs;
    for (auto mod : docs->docs.keys())
    {
        for (auto f : docs->docs[mod].keys())
        {
            fs[f] = docs->docs[mod][f].doc;
        }
    }

    for (auto f : fs.keys())
    {
        const auto doc = fs[f];

        auto f_ = doc.count(" ") ? doc.split(" ")[0] : "";
        if (f_ != f)
        {
            if (fs.count(f_) != 1)
            {
                std::cerr << "DocumentationPane: missing alias "
                          << f_.toStdString() << " for " << f.toStdString()
                          << std::endl;
            }
            txt->insertPlainText(f);
            txt->insertPlainText(": alias for " + f_ + "\n\n");
        }
        else
        {
            txt->insertPlainText(doc + "\n\n");
        }
    }

    layout->addWidget(txt);

    setLayout(layout);
    setWindowFlags(Qt::Tool | Qt::CustomizeWindowHint |
                   Qt::WindowTitleHint | Qt::WindowCloseButtonHint);
    setAttribute(Qt::WA_DeleteOnClose);
}

void DocumentationPane::setDocs(Documentation* ds)
{
    docs.reset(ds);
}

bool DocumentationPane::hasDocs()
{
    return docs.data();
}
