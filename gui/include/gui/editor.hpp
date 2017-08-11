#include <QTextEdit>
#include <QPlainTextEdit>

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

signals:
    void scriptChanged(QString s);
    void modificationChanged(bool m);

protected:
    void setResult(bool valid, QString result);
    void setError(QPair<uint32_t, uint32_t> begin,
                  QPair<uint32_t, uint32_t> end);
    QList<QTextEdit::ExtraSelection> clearError(bool set=true);

    QTextEdit* txt;
    QPlainTextEdit* err;
    Syntax* syntax;

    QTextCharFormat error_format;
};
