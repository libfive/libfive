#include <QDesktopWidget>
#include <QFileDialog>
#include <QProgressDialog>
#include <QSplitter>
#include <QMenuBar>
#include <QMessageBox>

#include "gui/window.hpp"
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

Window::Window(const QString& target)
    : QMainWindow(), editor(new Editor), view(new View)
{
    resize(QDesktopWidget().availableGeometry(this).size() * 0.75);

    auto layout = new QSplitter();
    layout->addWidget(editor);
    editor->resize(width() * 0.4, editor->height());
    layout->addWidget(view);
    setCentralWidget(layout);

    // Sync document modification state with window
    connect(editor, &Editor::modificationChanged,
            this, &QWidget::setWindowModified);

    // Sync settings with script and vice versa
    // (the editor breaks the loop by not re-emitting the signal
    // if no changes are necessary to the block comment)
    connect(view, &View::settingsChanged,
            editor, &Editor::onSettingsChanged);
    connect(editor, &Editor::settingsChanged,
            view, &View::onSettingsFromScript);

    // File menu
    auto file_menu = menuBar()->addMenu("&File");

    auto new_action = file_menu->addAction("New");
    new_action->setShortcut(QKeySequence::New);
    connect(new_action, &QAction::triggered, this, &Window::onNew);

    auto open_action = file_menu->addAction("Open...");
    open_action->setShortcut(QKeySequence::Open);
    connect(open_action, &QAction::triggered, this, &Window::onOpen);

    file_menu->addSeparator();

    auto save_action = file_menu->addAction("Save");
    save_action->setShortcut(QKeySequence::Save);
    connect(save_action, &QAction::triggered, this, &Window::onSave);

    auto save_as_action = file_menu->addAction("Save As...");
    save_as_action->setShortcut(QKeySequence::SaveAs);
    connect(save_as_action, &QAction::triggered, this, &Window::onSaveAs);

    file_menu->addSeparator();

    auto export_action = file_menu->addAction("Export...");
    connect(export_action, &QAction::triggered, this, &Window::onExport);

    // Settings menu
    auto view_menu = menuBar()->addMenu("&View");
    connect(view_menu->addAction("Bounds / resolution"), &QAction::triggered,
            view, &View::openSettings);

    auto show_axes_action = view_menu->addAction("Show axes");
    show_axes_action->setCheckable(true);
    show_axes_action->setChecked(true);
    connect(show_axes_action, &QAction::triggered,
            view, &View::showAxes);

    // Help menu
    auto help_menu = menuBar()->addMenu("Help");
    connect(help_menu->addAction("About"), &QAction::triggered,
            this, &Window::onAbout);

    // Start embedded Guile interpreter
    auto interpreter = new Interpreter();
    connect(editor, &Editor::scriptChanged,
            interpreter, &Interpreter::onScriptChanged);

    connect(interpreter, &Interpreter::gotResult, editor, &Editor::onResult);
    connect(interpreter, &Interpreter::gotError, editor, &Editor::onError);
    connect(interpreter, &Interpreter::keywords, editor, &Editor::setKeywords);
    connect(interpreter, &Interpreter::gotShapes, view, &View::setShapes);

    interpreter->start();

    show();

    if (loadFile(target))
    {
        setFilename(target);
    }
}

////////////////////////////////////////////////////////////////////////////////

void Window::onOpen(bool)
{
    CHECK_UNSAVED();

    QString f = QFileDialog::getOpenFileName(nullptr, "Open", "", "*.ao");
    if (!f.isEmpty() && loadFile(f))
    {
        setFilename(f);
    }
}

bool Window::loadFile(QString f)
{
    QFile file(f);
    if (!file.open(QIODevice::ReadOnly))
    {
        QMessageBox m(this);
        m.setText("Failed to open file");
        m.setInformativeText("<code>" + f + "</code><br>does not exist");
        m.addButton(QMessageBox::Ok);
        m.setIcon(QMessageBox::Critical);
        m.setWindowModality(Qt::WindowModal);
        m.exec();
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

bool Window::saveFile(QString f)
{
    QFile file(f);
    if (!QFileInfo(QFileInfo(f).path()).isWritable())
    {

        QMessageBox m(this);
        m.setText("Failed to save file");
        m.setInformativeText("<code>" + f + "</code><br>is not writable");
        m.addButton(QMessageBox::Ok);
        m.setIcon(QMessageBox::Critical);
        m.setWindowModality(Qt::WindowModal);
        m.exec();
        return false;
    }
    if (!file.open(QIODevice::WriteOnly))
    {
        QMessageBox m(this);
        m.setText("Failed to save file");
        m.setInformativeText("<code>" + f + "</code><br>does not exist");
        m.addButton(QMessageBox::Ok);
        m.setIcon(QMessageBox::Critical);
        m.setWindowModality(Qt::WindowModal);
        m.exec();
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

void Window::onSave(bool)
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

void Window::onSaveAs(bool)
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

void Window::onNew(bool)
{
    CHECK_UNSAVED();

    setFilename("");
    editor->setScript("");
    editor->setModified(false);
}

void Window::closeEvent(QCloseEvent* event)
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

QMessageBox::StandardButton Window::checkUnsaved()
{
    if (isWindowModified())
    {
        QMessageBox m(this);
        m.setText("Do you want to save your changes to this document?");
        m.setInformativeText("If you don't save, your changes will be lost");
        m.addButton(QMessageBox::Discard);
        m.addButton(QMessageBox::Cancel);
        m.addButton(QMessageBox::Save);
        m.setIcon(QMessageBox::Warning);
        m.setWindowModality(Qt::WindowModal);
        return static_cast<QMessageBox::StandardButton>(m.exec());
    }
    else
    {
        return QMessageBox::Ok;
    }
}

void Window::setFilename(const QString& f)
{
    filename = f;
    setWindowFilePath(f);
}

////////////////////////////////////////////////////////////////////////////////

void Window::onExportReady(QList<const Kernel::Mesh*> shapes)
{
    disconnect(view, &View::meshesReady, this, &Window::onExportReady);
    if (!Kernel::Mesh::saveSTL(export_filename.toStdString(),
                               shapes.toStdList()))
    {
        QMessageBox m(this);
        m.setText("Could not save file");
        m.setInformativeText("Check the console for more information");
        m.addButton(QMessageBox::Ok);
        m.setIcon(QMessageBox::Critical);
        m.setWindowModality(Qt::WindowModal);
        m.exec();
    }
    emit(exportDone());
}

void Window::onExport(bool)
{
    export_filename = QFileDialog::getSaveFileName(
            nullptr, "Export", "", "*.stl");
    if (export_filename.isEmpty())
    {
        return;
    }

    connect(view, &View::meshesReady, this, &Window::onExportReady);

    auto p = new QProgressDialog(this);
    p->setCancelButton(nullptr);
    p->setWindowModality(Qt::WindowModal);
    p->setLabelText("Exporting...");
    p->setMaximum(0);
    connect(this, &Window::exportDone, p, &QProgressDialog::reset);
    p->show();

    view->checkMeshes();
}

////////////////////////////////////////////////////////////////////////////////

void Window::onAbout(bool)
{
    QString info = "A Scheme-based GUI for<br>the Ao CAD kernel<br><br>"
                   "<a href=\"https://github.com/mkeeter/ao\">Source on Github</a>";
#ifdef Q_OS_MAC
    QWidget a;
    QIcon icon(QCoreApplication::applicationDirPath() +
               "/../Resources/ao.icns");
    QMessageBox m(this);
    auto px = icon.pixmap(128);
    px.setDevicePixelRatio(devicePixelRatio());
    m.setIconPixmap(px);
    m.setText("Studio");
    m.setInformativeText(info);
    m.exec();
#else
    QMessageBox::about(this, "Studio",info);
#endif
}
