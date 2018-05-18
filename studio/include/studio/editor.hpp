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

#include <QPlainTextEdit>
#include <QTimer>

#include "studio/script.hpp"
#include "studio/settings.hpp"
#include "libfive/tree/tree.hpp"

class Syntax;

class Editor : public QWidget
{
    Q_OBJECT
public:
    Editor(QWidget* parent=nullptr, bool do_syntax=false);
    void setScript(const QString& s);
    QString getScript() const;
    void setModified(bool m);

    struct Range
    {
        Range() : Range(-1, -1, -1, -1) {}
        Range(int sr, int er, int sc, int ec) :
            start_row(sr), end_row(er), start_col(sc), end_col(ec) {}

        int start_row;
        int end_row;
        int start_col;
        int end_col;
    };

public slots:
    void onResult(QString result);
    void onError(QString result, QString stack, Range p);
    void onBusy();

    void undo();
    void redo();

    void setKeywords(QString kws);

    /*
     *  When settings are changed, add specially-formatted comment blocks
     *  to the script to control settings
     */
    void onSettingsChanged(Settings s);

    /*
     *  When script changes, emit scriptChanged and check for settings
     *  embedded in the script
     */
    void onScriptChanged();

    /*
     *  Stores the textual positions of variables
     */
    void setVarPositions(QMap<Kernel::Tree::Id, Range> vs)
    { vars = vs; }

    /*
     *  Modifies the textual values of variables
     */
    void setVarValues(QMap<Kernel::Tree::Id, float> vs);

    /*
     *  While a drag is taking place, the text field is frozen to user editing
     *  and drag_cursor is engaged in an edit session (so the entire drag
     *  operation can be undone at once).
     */
    void onDragStart();
    void onDragEnd();

signals:
    void scriptChanged(QString s);
    void modificationChanged(bool m);
    void undoAvailable(bool a);
    void redoAvailable(bool a);

    /*
     *  Invoked when a script defines settings using specially-formatted
     *  comment blocks.  first is true if this is the first time this script
     *  has been evaluated, false otherwise.
     */
    void settingsChanged(Settings s, bool first);

protected slots:
    void onSpinner();

protected:
    void setResult(QColor color, QString result);
    void setError(Range p);
    QList<QTextEdit::ExtraSelection> clearError(bool set=true);

    Script* script;
    QTextDocument* script_doc;
    Syntax* syntax;

    QPlainTextEdit* err;
    QTextDocument* err_doc;

    QTextCharFormat error_format;
    QTimer spinner;

    bool drag_should_join=false;
    bool first_change=false;

    QMap<Kernel::Tree::Id, Range> vars;
};
