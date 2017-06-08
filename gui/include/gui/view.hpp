#pragma once

#include <QOpenGLWidget>

#include "gui/axes.hpp"
#include "gui/background.hpp"

class View : public QOpenGLWidget
{
    Q_OBJECT
public:
    View(QWidget* parent=nullptr);

protected:
    void initializeGL() override;
    void paintGL() override;

    Axes axes;
    Background background;
};
