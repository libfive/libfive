#include <QTextEdit>

class Syntax;

class Editor : public QWidget
{
    Q_OBJECT
public:
    Editor(QWidget* parent=nullptr);
signals:
    void scriptChanged(QString s);
    void resultChanged(bool valid, QString result);
};
