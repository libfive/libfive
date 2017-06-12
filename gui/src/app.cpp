#include <QMainWindow>
#include <QDesktopWidget>
#include <QSplitter>
#include <QMenuBar>

#include "gui/app.hpp"
#include "gui/editor.hpp"
#include "gui/interpreter.hpp"
#include "gui/view.hpp"

App::App(int& argc, char** argv)
    : QApplication(argc, argv)
{
    auto window = new QMainWindow();
    window->resize(QDesktopWidget().availableGeometry(window).size() * 0.5);

    auto editor = new Editor();
    auto layout = new QSplitter();
    auto view = new View();
    layout->addWidget(editor);
    layout->addWidget(view);
    window->setCentralWidget(layout);

    auto file_menu = window->menuBar()->addMenu("&File");
    auto open_action = file_menu->addAction("&Open");
    connect(open_action, &QAction::triggered,
            [](bool){ qDebug() << "Opening!"; });
    auto view_menu = window->menuBar()->addMenu("&View");
    connect(view_menu->addAction("Bounds / resolution"), &QAction::triggered,
            view, [=](bool){ view->openSettings(); });

    auto interpreter = new Interpreter();
    connect(editor, &Editor::scriptChanged,
            interpreter, &Interpreter::onScriptChanged);
    connect(interpreter, &Interpreter::resultChanged,
            editor, &Editor::resultChanged);
    connect(interpreter, &Interpreter::keywords,
            editor, &Editor::keywords);
    connect(interpreter, &Interpreter::gotShape,
            view, &View::setShape);
    interpreter->start();

    view->openSettings();

    window->show();
}
