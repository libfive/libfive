#include <QApplication>

class Editor;

class App : public QApplication
{
    Q_OBJECT
public:
    explicit App(int& argc, char **argv);

protected slots:
    void onOpen(bool=false);
    void onSave(bool=false);
    void onSaveAs(bool=false);

protected:
    bool loadFile(QString f);
    bool saveFile(QString f);

    QString filename;
    Editor* editor;
};
