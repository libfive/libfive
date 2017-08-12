#include <QDesktopWidget>
#include <QFileDialog>
#include <QProgressDialog>
#include <QSplitter>
#include <QMenuBar>
#include <QMessageBox>

#include "gui/app.hpp"
#include "gui/editor.hpp"
#include "gui/interpreter.hpp"
#include "gui/view.hpp"

#define CHECK_UNSAVED() \
switch (checkUnsaved())                                         \
{                                                               \
    case QMessageBox::Save:     onSave();   /* FALLTHROUGH */   \
    case QMessageBox::Ok:                   /* FALLTHROUGH */   \
    case QMessageBox::Discard:  break;                          \
    case QMessageBox::Cancel:   return;                         \
    default:    assert(false);                                  \
}

App::App(int& argc, char **argv)
    : QMainWindow(), editor(new Editor), view(new View)
{
    resize(QDesktopWidget().availableGeometry(this).size() * 0.5);

    auto layout = new QSplitter();
    layout->addWidget(editor);
    layout->addWidget(view);
    setCentralWidget(layout);

    // Sync document modification state with window
    connect(editor, &Editor::modificationChanged,
            this, &QWidget::setWindowModified);

    auto file_menu = menuBar()->addMenu("&File");

    auto new_action = file_menu->addAction("New");
    new_action->setShortcut(QKeySequence::New);
    connect(new_action, &QAction::triggered, this, &App::onNew);

    auto open_action = file_menu->addAction("Open...");
    open_action->setShortcut(QKeySequence::Open);
    connect(open_action, &QAction::triggered, this, &App::onOpen);

    file_menu->addSeparator();

    auto save_action = file_menu->addAction("Save");
    save_action->setShortcut(QKeySequence::Save);
    connect(save_action, &QAction::triggered, this, &App::onSave);

    auto save_as_action = file_menu->addAction("Save As...");
    save_as_action->setShortcut(QKeySequence::SaveAs);
    connect(save_as_action, &QAction::triggered, this, &App::onSaveAs);

    file_menu->addSeparator();

    auto export_action = file_menu->addAction("Export...");
    connect(export_action, &QAction::triggered, this, &App::onExport);

    auto view_menu = menuBar()->addMenu("&View");
    connect(view_menu->addAction("Bounds / resolution"), &QAction::triggered,
            view, &View::openSettings);

    auto show_axes_action = view_menu->addAction("Show axes");
    show_axes_action->setCheckable(true);
    show_axes_action->setChecked(true);
    connect(show_axes_action, &QAction::triggered,
            view, &View::showAxes);

    auto interpreter = new Interpreter();
    connect(editor, &Editor::scriptChanged,
            interpreter, &Interpreter::onScriptChanged);

    connect(interpreter, &Interpreter::gotResult, editor, &Editor::onResult);
    connect(interpreter, &Interpreter::gotError, editor, &Editor::onError);
    connect(interpreter, &Interpreter::keywords, editor, &Editor::setKeywords);
    connect(interpreter, &Interpreter::gotShapes, view, &View::setShapes);

    interpreter->start();

    show();

    if (argc > 1 && loadFile(argv[1]))
    {
        setFilename(argv[1]);
    }
}

////////////////////////////////////////////////////////////////////////////////

void App::onOpen(bool)
{
    CHECK_UNSAVED();

    QString f = QFileDialog::getOpenFileName(nullptr, "Open", "", "*.ao");
    if (!f.isEmpty() && loadFile(f))
    {
        setFilename(f);
    }
}

bool App::loadFile(QString f)
{
    QFile file(f);
    if (!file.open(QIODevice::ReadOnly))
    {
        QMessageBox::critical(NULL, "Loading error",
                "<b>Loading error:</b><br>"
                "File does not exist.");
        return false;
    }
    else
    {
        editor->setScript(file.readAll());
        editor->setModified(false);
        return true;
    }
}

////////////////////////////////////////////////////////////////////////////////

bool App::saveFile(QString f)
{
    QFile file(f);
    if (!QFileInfo(QFileInfo(f).path()).isWritable())
    {
        QMessageBox::critical(NULL, "Save As error",
                "<b>Save As error:</b><br>"
                "Target file is not writable.");
        return false;
    }
    if (!file.open(QIODevice::WriteOnly))
    {
        QMessageBox::critical(nullptr, "Save error",
                "<b>Error while saving:</b><br>"
                "File does not exist.");
        return false;
    }
    else
    {
        QTextStream out(&file);
        out << editor->getScript();

        editor->setModified(false);
        return true;
    }
}

void App::onSave(bool)
{
    if (filename.isEmpty())
    {
        onSaveAs();
    }
    else
    {
        saveFile(filename);
    }
}

void App::onSaveAs(bool)
{
    QString f = QFileDialog::getSaveFileName(nullptr, "Save as", "", "*.ao");
    if (!f.isEmpty())
    {
#ifdef Q_OS_LINUX
        if (!f.endsWith(".ao"))
        {
            f += ".ao";
        }
#endif
        if (saveFile(f))
        {
            setFilename(f);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

void App::onNew(bool)
{
    CHECK_UNSAVED();

    setFilename("");
    editor->setScript("");
    editor->setModified(false);
}

void App::closeEvent(QCloseEvent* event)
{
    if (closing)
    {
        event->accept();
    }
    else
    {
        switch (checkUnsaved())
        {
            case QMessageBox::Save:     onSave();   /* FALLTHROUGH */
            case QMessageBox::Ok:                   /* FALLTHROUGH */
            case QMessageBox::Discard:  event->accept(); break;
            case QMessageBox::Cancel:   event->ignore(); break;
            default:                    assert(false);
        }
        closing = event->isAccepted();
    }
}

////////////////////////////////////////////////////////////////////////////////

QMessageBox::StandardButton App::checkUnsaved()
{
    if (isWindowModified())
    {
        auto m = new QMessageBox(this);
        m->setText("Do you want to save your changes to this document?");
        m->setInformativeText("If you don't save, your changes will be lost");
        m->addButton(QMessageBox::Discard);
        m->addButton(QMessageBox::Cancel);
        m->addButton(QMessageBox::Save);
        m->setIcon(QMessageBox::Warning);
        m->setWindowModality(Qt::WindowModal);
        auto r = static_cast<QMessageBox::StandardButton>(m->exec());
        delete m;
        return r;
    }
    else
    {
        return QMessageBox::Ok;
    }
}

void App::setFilename(const QString& f)
{
    filename = f;
    setWindowFilePath(f);
}

////////////////////////////////////////////////////////////////////////////////

void App::onExportReady(QList<const Kernel::Mesh*> shapes)
{
    disconnect(view, &View::meshesReady, this, &App::onExportReady);
    emit(exportDone());
}

void App::onExport(bool)
{
    //QString f = QFileDialog::getSaveFileName(nullptr, "Export", "", "*.stl");
    //if (f.isEmpty())
    {
//        return;
    }

    connect(view, &View::meshesReady, this, &App::onExportReady);

    auto p = new QProgressDialog(this);
    p->setCancelButton(nullptr);
    p->setWindowModality(Qt::WindowModal);
    p->setLabelText("Exporting...");
    p->setMaximum(0);
    connect(this, &App::exportDone, [](){ qDebug() << "Ending"; });
    connect(this, &App::exportDone, p, &QProgressDialog::reset);
    p->show();

    view->checkMeshes();
}
