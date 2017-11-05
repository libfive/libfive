#pragma once

#include <QWidget>
#include <QString>
#include <QMap>

struct Documentation
{
public:
    struct Function {
        /*  One or the other should be populated here */
        QString doc;
        QString alias;
    };

    /*
     *  Stores a new function in the documentation
     */
    void insert(QString module, QString name, QString doc);

    /*
     *  Looks through the docs to find functions whose docstrings
     *  refer to a different function name, then marks them as aliases
     *
     *  Prints a warning if there is a broken alias
     */
    void buildAliases();

    /*  Map from module name to map of functions */
    QMap<QString, QMap<QString, Function>> docs;
};

////////////////////////////////////////////////////////////////////////////////

class DocumentationPane : public QWidget
{
    Q_OBJECT
public:
    /*
     *  We can only call this constructor after setDocs has been called
     *  (throws an assertion otherwise)
     */
    DocumentationPane();

    static void setDocs(Documentation* ds);
    static bool hasDocs();

protected:
    /*  Take ownership of the documentation  */
    static QScopedPointer<Documentation> docs;
};
