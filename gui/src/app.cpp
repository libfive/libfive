#include <QMainWindow>

#include "gui/app.hpp"
#include "gui/editor.hpp"

App::App(int& argc, char** argv)
    : QApplication(argc, argv)
{
    auto window = new QMainWindow();
    window->setCentralWidget(new Editor(window));
    window->show();
}
