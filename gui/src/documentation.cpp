#include <iostream>

#include <QTextBrowser>
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

    // Flatten documentation into a single-level map
    QMap<QString, QString> fs;
    QMap<QString, QString> tags;
    for (auto mod : docs->docs.keys())
    {
        for (auto f : docs->docs[mod].keys())
        {
            fs[f] = docs->docs[mod][f].doc;
            tags.insert(f, "i" + QString::fromStdString(
                        std::to_string(tags.size())));
        }
    }

    // Unpack documentation into a text box
    auto txt = new QTextBrowser();
    for (auto f : fs.keys())
    {
        const auto doc = fs[f];

        auto f_ = doc.count(" ") ? doc.split(" ")[0] : "";
        txt->insertHtml("<a name=\"" + tags[f] + "\" href=\"#" + tags[f] + "\">" + f + "</a><br>");
        if (f_ != f)
        {
            txt->insertPlainText(f);
            txt->insertPlainText(": alias for ");
            if (fs.count(f_) != 1)
            {
                std::cerr << "DocumentationPane: missing alias "
                          << f_.toStdString() << " for " << f.toStdString()
                          << std::endl;
                txt->insertPlainText(f_ + " (missing)\n");
            }
            else
            {
                txt->insertHtml("<a href=\"#" + tags[f_] + "\">" + f_ + "</a><br>");
            }
            txt->insertPlainText("\n");
        }
        else
        {
            txt->insertPlainText(doc + "\n\n");
        }
    }
    txt->setReadOnly(true);
    txt->setTextInteractionFlags(Qt::LinksAccessibleByMouse);

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
