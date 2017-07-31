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
    void onResultChanged(bool valid, QString result);

signals:
    void scriptChanged(QString s);
    void keywords(QString kws);

protected:
    QTextEdit* txt;
    QPlainTextEdit* err;
};
