#include <QTextEdit>
#include <QPlainTextEdit>
#include <QTimer>

#include "gui/settings.hpp"

class Syntax;

class Editor : public QWidget
{
    Q_OBJECT
public:
    Editor(QWidget* parent=nullptr);
    void setScript(const QString& s);
    QString getScript() const;
    void setModified(bool m);

public slots:
    void onResult(QString result);
    void onError(QString result, QPair<uint32_t, uint32_t> begin,
                                 QPair<uint32_t, uint32_t> end);
    void onBusy();
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

signals:
    void scriptChanged(QString s);
    void modificationChanged(bool m);

    /*
     *  Invoked when a script defines settings using specially-formatted
     *  comment blocks
     */
    void settingsChanged(Settings s);

protected slots:
    void onSpinner();

protected:
    void setResult(QColor color, QString result);
    void setError(QPair<uint32_t, uint32_t> begin,
                  QPair<uint32_t, uint32_t> end);
    QList<QTextEdit::ExtraSelection> clearError(bool set=true);

    QTextEdit* script;
    QTextDocument* script_doc;
    Syntax* syntax;

    QPlainTextEdit* err;
    QTextDocument* err_doc;

    QTextCharFormat error_format;
    QTimer spinner;
};
