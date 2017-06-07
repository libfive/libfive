#pragma once

#include <QOpenGLWidget>

class View : public QOpenGLWidget
{
    Q_OBJECT
public:
    View(QWidget* parent=nullptr);
};
