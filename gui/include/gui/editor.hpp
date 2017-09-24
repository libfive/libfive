#pragma once

#include <QTextEdit>
#include <QPlainTextEdit>
#include <QTimer>

#include "gui/settings.hpp"
#include "ao/tree/tree.hpp"

class Syntax;

class Editor : public QWidget
{
    Q_OBJECT
public:
    Editor(QWidget* parent=nullptr);
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

    QTextEdit* script;
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
