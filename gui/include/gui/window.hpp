#pragma once

#include <QApplication>
#include <QMainWindow>
#include <QMessageBox>

class Editor;
class View;
namespace Kernel { class Mesh; }

class Window : public QMainWindow
{
    Q_OBJECT
public:
    explicit Window(const QString& target);

    /*
     *  Loads a file by path, properly checking if the existing document
     *  is unsaved and asking the user to save it in that case.
     */
    void openFile(const QString& name);

protected slots:
    void onOpen(bool=false);
    void onSave(bool=false);
    void onSaveAs(bool=false);
    void onNew(bool=false);
    void onExport(bool=false);
    void onAbout(bool=false);

signals:
    void exportDone();

protected:
    void closeEvent(QCloseEvent* event) override;
    QMessageBox::StandardButton checkUnsaved();
    void setFilename(const QString& str);

    bool loadFile(QString f);
    bool saveFile(QString f);

    void onExportReady(QList<const Kernel::Mesh*> shapes);

    /*  Filename of the current file, or empty string */
    QString filename;

    /*  Used to store the export target while meshes are being generated
     *  and the main event loop is blocked by a progress dialog */
    QString export_filename;

    Editor* editor;
    View* view;
    bool closing=false;
};
