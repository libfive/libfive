#include <QMainWindow>

#include "gui/app.hpp"
#include "gui/editor.hpp"
#include "gui/interpreter.hpp"

App::App(int& argc, char** argv)
    : QApplication(argc, argv)
{
    auto window = new QMainWindow();
    auto editor = new Editor(window);

    window->setCentralWidget(editor);

    auto interpreter = new Interpreter();
    connect(editor, &Editor::scriptChanged,
            interpreter, &Interpreter::onScriptChanged);
    connect(interpreter, &Interpreter::resultChanged,
            editor, &Editor::resultChanged);

    window->show();
}
