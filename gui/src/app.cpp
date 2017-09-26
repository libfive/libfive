#include <QFileOpenEvent>
#include <QDebug>

#include "gui/app.hpp"

App::App(int& argc, char** argv)
    : QApplication(argc, argv),
      window(argc > 1 ? argv[1] : ":/examples/tutorial.ao")
{
    // Nothing to do here
}

bool App::event(QEvent *event)
{
    if (event->type() == QEvent::FileOpen) {
        QFileOpenEvent *openEvent = static_cast<QFileOpenEvent*>(event);
        window.openFile(openEvent->file());
    }

    return QApplication::event(event);
}
