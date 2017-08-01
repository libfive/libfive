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

public slots:
    void onResult(QString result);
    void onError(QString result, QPair<uint32_t, uint32_t> begin,
                                 QPair<uint32_t, uint32_t> end);

signals:
    void scriptChanged(QString s);
    void keywords(QString kws);

protected:
    void setResult(bool valid, QString result);
    void setError(QPair<uint32_t, uint32_t> begin,
                  QPair<uint32_t, uint32_t> end);
    QList<QTextEdit::ExtraSelection> clearError(bool set=true);

    QTextEdit* txt;
    QPlainTextEdit* err;

    QTextCharFormat error_format;
};
