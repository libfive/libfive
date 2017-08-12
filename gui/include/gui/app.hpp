#include <QApplication>
#include <QMainWindow>
#include <QMessageBox>

class Editor;
class View;
namespace Kernel { class Mesh; }

class App : public QMainWindow
{
    Q_OBJECT
public:
    explicit App(int& argc, char **argv);

protected slots:
    void onOpen(bool=false);
    void onSave(bool=false);
    void onSaveAs(bool=false);
    void onNew(bool=false);
    void onExport(bool=false);

signals:
    void exportDone();

protected:
    void closeEvent(QCloseEvent* event) override;
    QMessageBox::StandardButton checkUnsaved();
    void setFilename(const QString& str);

    bool loadFile(QString f);
    bool saveFile(QString f);

    void onExportReady(QList<const Kernel::Mesh*> shapes);

    QString filename;
    Editor* editor;
    View* view;
    bool closing=false;
};
