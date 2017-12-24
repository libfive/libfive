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
#pragma once

#include <QWidget>
#include <QLineEdit>
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
    static void open();

protected:
    /*
     *  Special-case handling of escape (closes window)
     */
    bool eventFilter(QObject* object, QEvent* event) override;

    /*  Take ownership of the documentation  */
    static QScopedPointer<Documentation> docs;
    static QPointer<DocumentationPane> instance;

    QLineEdit* search;
};
