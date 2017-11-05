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
    for (auto mod : docs->docs.keys())
    {
        for (auto f : docs->docs[mod].keys())
        {
            txt->setFontWeight(14);
            txt->insertPlainText(f + "\n");
            txt->setFontWeight(12);
            txt->insertPlainText(docs->docs[mod][f].doc + "\n");
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
