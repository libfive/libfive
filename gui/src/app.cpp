#include <QMainWindow>
#include <QDesktopWidget>
#include <QFileDialog>
#include <QSplitter>
#include <QMenuBar>
#include <QMessageBox>

#include "gui/app.hpp"
#include "gui/editor.hpp"
#include "gui/interpreter.hpp"
#include "gui/view.hpp"

App::App(int& argc, char** argv)
    : QApplication(argc, argv), editor(new Editor)
{
    auto window = new QMainWindow;
    window->resize(QDesktopWidget().availableGeometry(window).size() * 0.5);

    auto layout = new QSplitter();
    auto view = new View();
    layout->addWidget(editor);
    layout->addWidget(view);
    window->setCentralWidget(layout);

    auto file_menu = window->menuBar()->addMenu("&File");

    auto open_action = file_menu->addAction("Open");
    open_action->setShortcut(QKeySequence::Open);
    connect(open_action, &QAction::triggered, this, &App::onOpen);

    auto save_action = file_menu->addAction("Save");
    save_action->setShortcut(QKeySequence::Save);
    connect(save_action, &QAction::triggered, this, &App::onSave);

    auto save_as_action = file_menu->addAction("Save As");
    save_as_action->setShortcut(QKeySequence::SaveAs);
    connect(save_as_action, &QAction::triggered, this, &App::onSaveAs);

    auto view_menu = window->menuBar()->addMenu("&View");
    connect(view_menu->addAction("Bounds / resolution"), &QAction::triggered,
            view, [=](bool){ view->openSettings(); });

    auto interpreter = new Interpreter();
    connect(editor, &Editor::scriptChanged,
            interpreter, &Interpreter::onScriptChanged);

    connect(interpreter, &Interpreter::gotResult, editor, &Editor::onResult);
    connect(interpreter, &Interpreter::gotError, editor, &Editor::onError);
    connect(interpreter, &Interpreter::keywords, editor, &Editor::setKeywords);
    connect(interpreter, &Interpreter::gotShapes, view, &View::setShapes);

    interpreter->start();

    window->show();

    if (argc > 1 && loadFile(argv[1]))
    {
        filename = argv[1];
    }
}

////////////////////////////////////////////////////////////////////////////////

void App::onOpen(bool)
{
    QString f = QFileDialog::getOpenFileName(nullptr, "Open", "", "*.ao");
    if (!f.isEmpty() && loadFile(f))
    {
        filename = f;
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
            filename = f;
        }
    }
}
