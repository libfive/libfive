#include <QMainWindow>
#include <QDesktopWidget>
#include <QSplitter>

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

    auto interpreter = new Interpreter();
    connect(editor, &Editor::scriptChanged,
            interpreter, &Interpreter::onScriptChanged);
    connect(interpreter, &Interpreter::resultChanged,
            editor, &Editor::resultChanged);

    window->show();
}
