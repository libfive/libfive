/*
Studio: a simple GUI for the Ao CAD kernel
Copyright (C) 2017  Matt Keeter

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
#include <QActionGroup>
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

Window::Window(QString target)
    : QMainWindow(), editor(new Editor), view(new View)
{
    resize(QDesktopWidget().availableGeometry(this).size() * 0.75);

    setAcceptDrops(true);

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

    // Connect drag start + end signals, so the user can't edit
    // the script while dragging in the 3D viewport
    connect(view, &View::dragStart, editor, &Editor::onDragStart);
    connect(view, &View::dragEnd, editor, &Editor::onDragEnd);

    // File menu
    auto file_menu = menuBar()->addMenu("&File");

    auto new_action = file_menu->addAction("New");
    new_action->setShortcut(QKeySequence::New);
    connect(new_action, &QAction::triggered, this, &Window::onNew);

    auto open_action = file_menu->addAction("Open...");
    open_action->setShortcut(QKeySequence::Open);
    connect(open_action, &QAction::triggered, this, &Window::onOpen);

    // Add a "Revert to saved" item, which is only enabled if there are
    // unsaved changes and there's an existing filename to load from.
    auto revert_action = file_menu->addAction("Revert to saved");
    connect(revert_action, &QAction::triggered, this, &Window::onRevert);
    connect(editor, &Editor::modificationChanged,
            revert_action, [=](bool changed){
                revert_action->setEnabled(
                        changed && !this->filename.isEmpty()); });
    revert_action->setEnabled(false);

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

    // Edit menu
    auto edit_menu = menuBar()->addMenu("&Edit");
    auto undo_action = edit_menu->addAction("Undo");
    undo_action->setEnabled(false);
    undo_action->setShortcut(QKeySequence::Undo);
    connect(undo_action, &QAction::triggered, editor, &Editor::undo);
    connect(editor, &Editor::undoAvailable, undo_action, &QAction::setEnabled);

    auto redo_action = edit_menu->addAction("Redo");
    redo_action->setEnabled(false);
    redo_action->setShortcut(QKeySequence::Redo);
    connect(redo_action, &QAction::triggered, editor, &Editor::redo);
    connect(editor, &Editor::redoAvailable, redo_action, &QAction::setEnabled);

    // Settings menu
    auto view_menu = menuBar()->addMenu("&View");
    auto show_axes_action = view_menu->addAction("Show axes");
    show_axes_action->setCheckable(true);
    show_axes_action->setChecked(true);
    connect(show_axes_action, &QAction::triggered,
            view, &View::showAxes);

    auto show_bbox_action = view_menu->addAction("Show bounding box(es)");
    show_bbox_action->setCheckable(true);
    connect(show_bbox_action, &QAction::triggered, view, &View::showBBox);

    auto perspective_action = new QAction("Perspective");
    auto ortho_action = new QAction("Orthographic");
    view_menu->addSection("Projection");
    view_menu->addAction(perspective_action);
    view_menu->addAction(ortho_action);
    perspective_action->setCheckable(true);
    perspective_action->setChecked(true);
    ortho_action->setCheckable(true);
    auto projection = new QActionGroup(view_menu);
    projection->addAction(perspective_action);
    projection->addAction(ortho_action);
    connect(perspective_action, &QAction::triggered,
            view, &View::toPerspective);
    connect(ortho_action, &QAction::triggered,
            view, &View::toOrthographic);

    view_menu->addSeparator();
    auto zoom_to_action = new QAction("Zoom to bounds");
    view_menu->addAction(zoom_to_action);
    connect(zoom_to_action, &QAction::triggered, view, &View::zoomTo);

    // Help menu
    auto help_menu = menuBar()->addMenu("Help");
    connect(help_menu->addAction("About"), &QAction::triggered,
            this, &Window::onAbout);
    connect(help_menu->addAction("Load tutorial"), &QAction::triggered,
            this, &Window::onLoadTutorial);

    // Start embedded Guile interpreter
    auto interpreter = new Interpreter();
    connect(editor, &Editor::scriptChanged,
            interpreter, &Interpreter::onScriptChanged);

    connect(interpreter, &Interpreter::busy, editor, &Editor::onBusy);
    connect(interpreter, &Interpreter::gotResult, editor, &Editor::onResult);
    connect(interpreter, &Interpreter::gotError, editor, &Editor::onError);
    connect(interpreter, &Interpreter::keywords, editor, &Editor::setKeywords);
    connect(interpreter, &Interpreter::gotShapes, view, &View::setShapes);
    connect(interpreter, &Interpreter::gotVars,
            editor, &Editor::setVarPositions);
    connect(view, &View::varsDragged, editor, &Editor::setVarValues);

    interpreter->start();

    show();

    {   //  Load the tutorial file on first run if there's no target
        QSettings settings("impraxical", "Studio");
        if (settings.contains("first-run") &&
            settings.value("first-run").toBool() &&
            target.isNull())
        {
            target = ":/examples/tutorial.ao";
        }
        settings.setValue("first-run", false);
    }

    if (!target.isEmpty() && loadFile(target))
    {
        setFilename(target);
    }
}

////////////////////////////////////////////////////////////////////////////////

void Window::openFile(const QString& name)
{
    CHECK_UNSAVED();

    if (loadFile(name))
    {
        setFilename(name);
    }
}

void Window::onOpen(bool)
{
    CHECK_UNSAVED();

    QString f = QFileDialog::getOpenFileName(nullptr, "Open", "", "*.ao");
    if (!f.isEmpty() && loadFile(f))
    {
        setFilename(f);
    }
}

void Window::onRevert(bool)
{
    CHECK_UNSAVED();
    Q_ASSERT(!filename.isEmpty());
    loadFile(filename);
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
    if (filename.isEmpty() || filename.startsWith(":/"))
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
        if (closing)
        {
            view->cancelShapes();
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

void Window::dragEnterEvent(QDragEnterEvent* event)
{
    if (event->mimeData()->hasUrls() &&
        event->mimeData()->urls().size() == 1 &&
        event->mimeData()->urls().front().fileName().toLower().endsWith(".ao"))
    {
        event->acceptProposedAction();
    }
}

void Window::dropEvent(QDropEvent* event)
{
    CHECK_UNSAVED();

    QString f = event->mimeData()->urls().first().path();
    if (!f.isEmpty() && loadFile(f))
    {
        setFilename(f);
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
    if (filename.startsWith(":/"))
    {
        setWindowTitle(QFileInfo(filename).fileName() + " (read-only)");
    }
    else
    {
        setWindowTitle(QString());
        setWindowFilePath(f);
    }
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
    view->disableSettings();

    auto p = new QProgressDialog(this);
    p->setCancelButton(nullptr);
    p->setWindowModality(Qt::WindowModal);
    p->setLabelText("Exporting...");
    p->setMaximum(0);

    // If we cancel the export (by pressing escape), then we shouldn't
    // run the final export step (of actually saving the meshes)
    connect(p, &QProgressDialog::rejected, this, [=](){
            disconnect(view, &View::meshesReady, this, &Window::onExportReady);
            this->export_filename = ""; });

    // Delete the progress dialog when we finish or cancel the export
    connect(this, &Window::exportDone, p, &QProgressDialog::deleteLater);
    connect(p, &QProgressDialog::rejected, p, &QProgressDialog::deleteLater);

    // When the progress dialog is destroyed, re-enable settings
    connect(p, &QProgressDialog::destroyed, view, &View::enableSettings);

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

void Window::onLoadTutorial(bool)
{
    CHECK_UNSAVED();

    QString target = ":/examples/tutorial.ao";
    if (loadFile(target))
    {
        setFilename(target);
    }
}
