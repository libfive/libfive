/*
Studio: a simple GUI for the libfive CAD kernel
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
#include <QFileSystemWatcher>

#include "studio/window.hpp"
#include "studio/documentation.hpp"
#include "studio/editor.hpp"
#include "studio/view.hpp"

#include "libfive.h"

#define CHECK_UNSAVED() \
switch (checkUnsaved())                                                     \
{                                                                           \
    case QMessageBox::Save:     if (!onSave()) return false;                \
    case QMessageBox::Ok:       /* FALLTHROUGH */                           \
    case QMessageBox::Discard:  break;                                      \
    case QMessageBox::Cancel:   return false;                               \
    default:    assert(false);                                              \
}

namespace Studio {

Window::Window(Arguments args)
    : QMainWindow(), editor(new Editor(args.language)), view(new View)
{
    resize(QDesktopWidget().availableGeometry(this).size() * 0.75);

    setAcceptDrops(true);

    auto layout = new QSplitter(args.vertical ? Qt::Vertical : Qt::Horizontal);

    if (args.vertical)
    {
        layout->addWidget(view);
        layout->addWidget(editor);
    }
    else
    {
        layout->addWidget(editor);
        layout->addWidget(view);
    }

    editor->resize(width() * 0.4, editor->height());
    setCentralWidget(layout);

    // Sync document modification state with window
    connect(editor, &Editor::modificationChanged,
            this, &QWidget::setWindowModified);

    // Sync settings from script to viewport
    connect(editor, &Editor::settingsChanged,
            view, &View::onSettingsFromScript);

    // Always run the autoload check, looking at the local
    // variable to decide whether or not to actually
    // reload the file.
    connect(&watcher, &QFileSystemWatcher::fileChanged,
            this, &Window::onAutoLoad);
    connect(&watcher, &QFileSystemWatcher::directoryChanged,
            this, &Window::onAutoLoadPath);


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

    auto open_viewer = file_menu->addAction("Open as viewer...");
    connect(open_viewer, &QAction::triggered, this, &Window::onOpenViewer);

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

    auto export_action = file_menu->addAction("Export STL...");
    export_action->setShortcuts({Qt::CTRL + Qt::Key_E, Qt::Key_F7});
    connect(export_action, &QAction::triggered, this, &Window::onExport);

    file_menu->addSeparator();

    auto quit_action = file_menu->addAction("Quit");
    quit_action->setShortcuts(QKeySequence::Quit);
    connect(quit_action, &QAction::triggered, this, &Window::onQuit);

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

    edit_menu->addSeparator();

    auto autoload_action = edit_menu->addAction("Automatically reload changes");
    connect(autoload_action, &QAction::triggered, this,
            [&](bool b) { autoreload = b; });
    autoload_action->setCheckable(true);

    connect(this, &Window::setAutoload, autoload_action,
            [=](bool b) { autoload_action->setChecked(b);
                          this->autoreload = b; });

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

    auto perspective_action = new QAction("Perspective", nullptr);
    auto ortho_action = new QAction("Orthographic", nullptr);
    auto proj_menu = new QMenu("Projection");
    proj_menu->addAction(perspective_action);
    proj_menu->addAction(ortho_action);
    perspective_action->setCheckable(true);
    perspective_action->setChecked(true);
    ortho_action->setCheckable(true);
    auto projection = new QActionGroup(proj_menu);
    projection->addAction(perspective_action);
    projection->addAction(ortho_action);
    connect(perspective_action, &QAction::triggered,
            view, &View::toPerspective);
    connect(ortho_action, &QAction::triggered,
            view, &View::toOrthographic);
    view_menu->addMenu(proj_menu);

    auto turn_z_up = new QAction("Turntable (Z up)", nullptr);
    auto turn_y_up = new QAction("Turntable (Y up)", nullptr);
    auto rotation_menu = new QMenu("Rotation mode");
    rotation_menu->addAction(turn_z_up);
    rotation_menu->addAction(turn_y_up);
    turn_z_up->setCheckable(true);
    turn_z_up->setChecked(true);
    turn_y_up->setCheckable(true);
    auto rot_mode = new QActionGroup(rotation_menu);
    rot_mode->addAction(turn_z_up);
    rot_mode->addAction(turn_y_up);
    connect(turn_z_up, &QAction::triggered, view, &View::toTurnZ);
    connect(turn_y_up, &QAction::triggered, view, &View::toTurnY);
    view_menu->addMenu(rotation_menu);

    auto sensitivity_low = new QAction("Low", nullptr);
    auto sensitivity_medium = new QAction("Medium", nullptr);
    auto sensitivity_high = new QAction("High", nullptr);
    auto sensitivity_menu = new QMenu("Rotation sensitivity");
    sensitivity_menu->addAction(sensitivity_low);
    sensitivity_menu->addAction(sensitivity_medium);
    sensitivity_menu->addAction(sensitivity_high);
    sensitivity_low->setCheckable(true);
    sensitivity_medium->setCheckable(true);
    sensitivity_medium->setChecked(true);
    sensitivity_high->setCheckable(true);
    auto sense_mode = new QActionGroup(sensitivity_menu);
    sense_mode->addAction(sensitivity_low);
    sense_mode->addAction(sensitivity_medium);
    sense_mode->addAction(sensitivity_high);
    connect(sensitivity_low, &QAction::triggered, view, &View::setLowRotSensitivity);
    connect(sensitivity_medium, &QAction::triggered, view, &View::setMedRotSensitivity);
    connect(sensitivity_high, &QAction::triggered, view, &View::setHighRotSensitivity);
    view_menu->addMenu(sensitivity_menu);

    auto cursor_centric = new QAction("Cursor", nullptr);
    auto scene_centric = new QAction("Scene", nullptr);
    auto zoom_menu = new QMenu("Zoom center");
    zoom_menu->addAction(cursor_centric);
    zoom_menu->addAction(scene_centric);
    cursor_centric->setCheckable(true);
    scene_centric->setCheckable(true);
    cursor_centric->setChecked(true);
    auto zoom_mode = new QActionGroup(zoom_menu);
    zoom_mode->addAction(cursor_centric);
    zoom_mode->addAction(scene_centric);
    connect(cursor_centric, &QAction::triggered, view, &View::setZoomCursorCentric);
    connect(scene_centric, &QAction::triggered, view, &View::setZoomSceneCentric);
    view_menu->addMenu(zoom_menu);

    view_menu->addSeparator();
    auto zoom_to_action = new QAction("Zoom to bounds", nullptr);
    view_menu->addAction(zoom_to_action);
    connect(zoom_to_action, &QAction::triggered, view, &View::zoomTo);

    // Debug menu
    auto debug_menu = menuBar()->addMenu("Debug");
    auto dc_meshing = new QAction("Dual contouring", nullptr);
    auto iso_meshing = new QAction("Iso-simplex", nullptr);
    auto hybrid_meshing = new QAction("Hybrid", nullptr);
    auto meshing_menu = new QMenu("Meshing algorithm");
    auto meshing_mode = new QActionGroup(meshing_menu);
    debug_menu->addMenu(meshing_menu);

    for (auto& m : { dc_meshing, iso_meshing, hybrid_meshing }) {
        meshing_menu->addAction(m);
        meshing_mode->addAction(m);
        m->setCheckable(true);
    }
    dc_meshing->setChecked(true);
    connect(dc_meshing, &QAction::triggered, view, &View::toDCMeshing);
    connect(iso_meshing, &QAction::triggered, view, &View::toIsoMeshing);
    connect(hybrid_meshing, &QAction::triggered, view, &View::toHybridMeshing);

    auto lang_menu = menuBar()->addMenu("Language");
    auto lang_guile = new QAction("Guile Scheme", nullptr);
    auto lang_python = new QAction("Python", nullptr);
    auto lang_group = new QActionGroup(lang_menu);
    for (auto& m : { lang_guile, lang_python }) {
        lang_menu->addAction(m);
        lang_group->addAction(m);
        m->setCheckable(true);
    }
    lang_menu->addSeparator();
    auto lang_load_default = new QAction("Load default script", nullptr);
    lang_menu->addAction(lang_load_default);
    switch (editor->getLanguage()) {
        case Language::LANGUAGE_GUILE: lang_guile->setChecked(true); break;
        case Language::LANGUAGE_PYTHON: lang_python->setChecked(true); break;
        case Language::LANGUAGE_NONE: break;
    }
    connect(lang_guile, &QAction::triggered,
            this, [=](bool){ setLanguage(Language::LANGUAGE_GUILE); });
    connect(lang_python, &QAction::triggered,
            this, [=](bool){ setLanguage(Language::LANGUAGE_PYTHON); });
    connect(lang_load_default, &QAction::triggered,
            this, &Window::onLoadDefault);
    if (!Editor::supportsLanguage(Language::LANGUAGE_GUILE)) {
        lang_guile->setEnabled(false);
    }
    if (!Editor::supportsLanguage(Language::LANGUAGE_PYTHON)) {
        lang_python->setEnabled(false);
    }

    // Help menu
    auto help_menu = menuBar()->addMenu("Help");
    connect(help_menu->addAction("About"), &QAction::triggered,
            this, &Window::onAbout);
    connect(help_menu->addAction("Load tutorial"), &QAction::triggered,
            this, &Window::onLoadTutorial);
    auto ref_action = help_menu->addAction("Shape reference");
    ref_action->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_Slash));
    connect(ref_action, &QAction::triggered, editor, &Editor::onShowDocs);

    // Link up the editor and the view
    connect(editor, &Editor::shapes, view, &View::setShapes);
    connect(view, &View::varsDragged, editor, &Editor::setVarValues);

    #if defined(Q_OS_LINUX) || defined(Q_OS_WIN)
        setWindowTitle("Studio[*]");
    #endif
    show();

    {   //  Load the tutorial file on first run if there's no target
        QSettings settings("impraxical", "Studio");
        if (settings.contains("first-run") &&
            settings.value("first-run").toBool() &&
            args.filename.isEmpty())
        {
            args.filename = ":/examples/tutorial.io";
        }
        settings.setValue("first-run", false);
    }

    onNew();
    if (!args.filename.isEmpty() && loadFile(args.filename))
    {
        setFilename(args.filename);
    }
}

////////////////////////////////////////////////////////////////////////////////

bool Window::openFile(const QString& name)
{
    CHECK_UNSAVED();

    if (loadFile(name))
    {
        setFilename(name);
        return true;
    }
    emit(setAutoload(false));
    return false;
}

bool Window::onOpen(bool)
{
    CHECK_UNSAVED();

    QString f = QFileDialog::getOpenFileName(this, "Open",
            workingDirectory(), "Studio (*.io *.py);;Any files (*)");
    if (!f.isEmpty())
    {
        return openFile(f);
    }
    return false;
}

bool Window::onOpenViewer(bool)
{
    auto result = onOpen();
    if (result)
    {
        emit(setAutoload(true));
    }
    return result;
}

bool Window::onRevert(bool)
{
    CHECK_UNSAVED();
    Q_ASSERT(!filename.isEmpty());
    return loadFile(filename);
}

bool Window::loadFile(QString f, bool reload)
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
        editor->setScript(file.readAll(), reload);
        editor->setModified(false);
        editor->guessLanguage(QFileInfo(file.fileName()).suffix().toLower());
        return true;
    }
}

////////////////////////////////////////////////////////////////////////////////

void Window::onAutoLoad(const QString&)
{
    if (QFile(filename).open(QIODevice::ReadOnly))
    {
        if (autoreload)
        {
            Q_ASSERT(!filename.isEmpty());
            loadFile(filename, true);
        }

        // Some editors don't edit but replace the file so the watcher thinks
        // the file was deleted and the signal is only sent once.
        // Re-watching the file is mandatory for those cases.
        if (QFile::exists(filename)) {
            watcher.addPath(filename);
        }
    }
    else // File was deleted. Waiting for new file
    {
        watcher.addPath(QFileInfo(filename).path());
    }
}

////////////////////////////////////////////////////////////////////////////////

void Window::onAutoLoadPath(const QString&)
{
    // file was created
    if (QFile(filename).open(QIODevice::ReadOnly))
    {
        watcher.removePaths(watcher.directories());
        watcher.addPath(filename);
        if (autoreload)
        {
            Q_ASSERT(!filename.isEmpty());
            loadFile(filename, true);
        }
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

bool Window::onSave(bool)
{
    if (filename.isEmpty() || filename.startsWith(":/"))
    {
        return onSaveAs();
    }
    else
    {
        return saveFile(filename);
    }
}

bool Window::onSaveAs(bool)
{
    QString f = QFileDialog::getSaveFileName(this, "Save as",
            workingDirectory(),
            QString("Studio (*%1);;Any files (*)").arg(editor->getExtension()));
    if (!f.isEmpty())
    {
        if (saveFile(f))
        {
            setFilename(f);
            return true;
        }
    }
    return false;
}

////////////////////////////////////////////////////////////////////////////////

bool Window::onNew(bool)
{
    return reset(Language::LANGUAGE_NONE);
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
        event->mimeData()->urls().size())
    {
        const auto f = event->mimeData()->urls().front().fileName().toLower();
        if (f.endsWith(".io") || f.endsWith(".ao"))
        {
            event->acceptProposedAction();
        }
    }
}

bool Window::dropEvent_(QDropEvent* event)
{
    CHECK_UNSAVED();

    QString f = event->mimeData()->urls().first().path();
    if (!f.isEmpty() && loadFile(f))
    {
        setFilename(f);
        return true;
    }
    return false;
}

void Window::dropEvent(QDropEvent* event)
{
    dropEvent_(event);
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
        QString title = QFileInfo(filename).fileName() + " (read-only)";
        #if defined(Q_OS_LINUX) || defined(Q_OS_WIN)
            setWindowTitle(title+"[*]");
        #else
            setWindowTitle(title);
        #endif
    }
    else
    {
        #if defined(Q_OS_LINUX) || defined(Q_OS_WIN)
            setWindowTitle(QFileInfo(filename).fileName() + "[*]");
        #else
            setWindowTitle(QString());
        #endif
        setWindowFilePath(f);
    }

    // Store the target file as our autoreload target
    // (even though we'll only do reloading if the menu option is set)
    auto watched_files = watcher.files();
    if (!watched_files.empty())
    {
        watcher.removePaths(watched_files);
    }
    if (!filename.startsWith(":/") && !filename.isEmpty())
    {
        watcher.addPath(filename);
    }
}

QString Window::workingDirectory() const
{
    return (filename.startsWith(":/") || filename.isEmpty())
        ? QDir::homePath()
        : QFileInfo(filename).dir().absolutePath();
}

////////////////////////////////////////////////////////////////////////////////

void Window::onExportReady(QList<const libfive::Mesh*> shapes)
{
    disconnect(view, &View::meshesReady, this, &Window::onExportReady);
    if (!libfive::Mesh::saveSTL(export_filename.toStdString(),
                                std::list<const libfive::Mesh*>(shapes.begin(), shapes.end())))
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
            this, "Export STL", workingDirectory(), "STL files (*.stl);;Any files (*)");
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

    // If we cancel the export (by pressing escape), then we shouldn't
    // run the final export step (of actually saving the meshes)
    connect(p, &QProgressDialog::rejected, this, [=](){
            disconnect(view, &View::meshesReady, this, &Window::onExportReady);
            this->export_filename = ""; });

    // Delete the progress dialog when we finish or cancel the export
    connect(this, &Window::exportDone, p, &QProgressDialog::deleteLater);
    connect(p, &QProgressDialog::rejected, p, &QProgressDialog::deleteLater);

    p->show();
    view->checkMeshes();
}

////////////////////////////////////////////////////////////////////////////////

void Window::onAbout(bool)
{
    QString info = "A Scheme-based GUI for<br>the libfive CAD kernel<br><br>";

    info += strlen(libfive_git_version())
        ? "Version: <code>" + QString(libfive_git_version()) + "</code><br>"
        : "Branch: <code>" + QString(libfive_git_branch()) + "</code><br>";
    info += "Revision: <code>" + QString(libfive_git_revision()) + "</code><br><br>";

    info += "<a href=\"https://github.com/libfive/libfive\">Source on Github</a>";
#ifdef Q_OS_MAC
    QWidget a;
    QIcon icon(QCoreApplication::applicationDirPath() +
               "/../Resources/studio.icns");
    QMessageBox m(this);
    auto px = icon.pixmap(128);
    px.setDevicePixelRatio(devicePixelRatio());
    m.setIconPixmap(px);
    m.setText("Studio");
    m.setInformativeText(info);
    m.exec();
#else
    QMessageBox::about(this, "Studio", info);
#endif
}

bool Window::onLoadTutorial(bool)
{
    CHECK_UNSAVED();

    QString target = ":/examples/tutorial.io";
    if (loadFile(target))
    {
        setFilename(target);
        return true;
    }
    return false;
}

bool Window::onLoadDefault(bool)
{
    CHECK_UNSAVED();

    editor->loadDefaultScript();
    return true;
}

void Window::onQuit(bool)
{
    Window::close();
}

bool Window::reset(Language::Type t) {
    CHECK_UNSAVED();

    setFilename("");
    #if defined(Q_OS_LINUX) || defined(Q_OS_WIN)
        setWindowTitle("Studio[*]");
    #endif

    if (t != Language::LANGUAGE_NONE) {
        editor->setLanguage(t);
    }
    editor->loadDefaultScript();
    emit(setAutoload(false));
    return true;
}

bool Window::setLanguage(Language::Type t) {
    return reset(t);
}

}   // namespace Studio
