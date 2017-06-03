#include <QTextEdit>

class Syntax;

class Editor : public QTextEdit
{
    Q_OBJECT
public:
    Editor(QWidget* parent=nullptr);
signals:
    void scriptChanged(QString s);
};
