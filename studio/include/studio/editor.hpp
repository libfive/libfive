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
#include <QVBoxLayout>
#include <QTimer>

#include "studio/documentation.hpp"
#include "studio/result.hpp"
#include "studio/language.hpp"

namespace Studio {

class Script;

/*
 *  An Editor contains the script text editor window and an error pane
 */
class Editor : public QWidget
{
    Q_OBJECT
public:
    Editor(Language::Type language);
    void setScript(const QString& s, bool reload=false);
    QString getScript() const;
    void setModified(bool m);

    /*  Sets the script state to the default for the current language */
    void loadDefaultScript();

    /*  Loads an interpreter / syntax / formatter for the given language */
    void setLanguage(Language::Type type);

    /*  Returns the language currently loaded */
    Language::Type getLanguage() const;

    /*  Guesses the language type based on file extension, then calls
     *  setLanguage to load the appropriate interpreter */
    void guessLanguage(QString extension);

    /*  Returns an extension for the given language */
    QString getExtension() const;

    /*  Checks whether the given language is supported */
    static bool supportsLanguage(Language::Type type);

    /*  Returns a default language based on preprocessor macros */
    static Language::Type defaultLanguage();

public slots:
    void onInterpreterDone(Result result);
    void onInterpreterBusy();

    void undo();
    void redo();

    /*
     *  When the text-change debouce timer expires, grab the script and
     *  emit it with scriptChanged() so that the interpreter starts working.
     */
    void onTextChangedDebounce();

    /*
     *  Modifies the textual values of variables
     */
    void setVarValues(QMap<libfive::Tree::Id, float> vs);

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
     *  Emitted when the interpreter delivers us a list of shapes
     */
    void shapes(QList<Shape*> shapes);

    /*
     *  Shows the documentation pane attached to the language
     *  (forwarded to m_language)
     */
    void onShowDocs();

    /*
     *  Invoked when a script defines settings using special global functions.
     *  first is true if this is the first time this script has been evaluated,
     *  false otherwise; it is used to decide whether to zoom to shape bounds.
     */
    void settingsChanged(Settings s, bool first);

protected slots:
    void onSpinner();

    /*  Marks the whole script as changed, which triggers re-highlighting */
    void onSyntaxReady();

protected:
    void setResult(QColor color, QString result);

    Script* script;             // owned by layout
    QTextDocument* script_doc;  // owned by script

    QPlainTextEdit* err;        // owned by layout
    QTextDocument* err_doc;     // owned by err;

    QVBoxLayout* layout;    // owned by the widget itself

    // The Language manages the embedded interpreter
    QScopedPointer<Language> m_language;

    // The documentation pane, constructed when the interpreter is ready
    // then shown / hidden as needed
    QScopedPointer<DocumentationPane> doc_pane;

    QTextCharFormat error_format;
    QTimer spinner;

    // Debounces text changes, to avoid emitting too many signals
    QTimer m_textChangedDebounce;

    // Debounces the interpreter's "busy" signal to avoid UI jitter
    QTimer m_interpreterBusyDebounce;

    bool drag_should_join=false;
    bool first_change=false;

    QMap<libfive::Tree::Id, QRect> vars;
};

}   // namespace Studio
