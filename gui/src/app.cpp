#include <QFileOpenEvent>
#include <QDebug>

#include "gui/app.hpp"
#include "gui/window.hpp"

App::App(int argc, char** argv)
    : QApplication(argc, argv)
{
    new Window(argc > 1 ? argv[1] : ":/examples/tutorial.ao");
}

bool App::event(QEvent *event)
{
    if (event->type() == QEvent::FileOpen) {
        QFileOpenEvent *openEvent = static_cast<QFileOpenEvent*>(event);
        qDebug() << "Open file" << openEvent->file();
    }

    return QApplication::event(event);
}
