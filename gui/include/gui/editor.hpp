#include <QTextEdit>
#include <QPlainTextEdit>

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
    void setKeywords(QString kws);

    /*
     *  When settings are changed, add a line of meta-code
     *  to the script to define them in-line.
     */
    void onSettingsChanged(Settings s);

signals:
    void scriptChanged(QString s);
    void modificationChanged(bool m);

protected:
    void setResult(bool valid, QString result);
    void setError(QPair<uint32_t, uint32_t> begin,
                  QPair<uint32_t, uint32_t> end);
    QList<QTextEdit::ExtraSelection> clearError(bool set=true);

    QTextEdit* script;
    QTextDocument* script_doc;
    Syntax* syntax;

    QPlainTextEdit* err;
    QTextDocument* err_doc;

    QTextCharFormat error_format;
};
