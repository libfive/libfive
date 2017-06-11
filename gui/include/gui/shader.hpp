#pragma once

#include <QOpenGLShaderProgram>

namespace Shader
{
    void initializeGL();
    extern QOpenGLShaderProgram* flat;
    extern QOpenGLShaderProgram* shaded;
}
