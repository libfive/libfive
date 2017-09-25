#pragma once

#include <QOpenGLShaderProgram>

namespace Shader
{
    void initializeGL();

    extern QOpenGLShaderProgram* flat;
    extern QOpenGLShaderProgram* monochrome;
    extern QOpenGLShaderProgram* shaded;
    extern QOpenGLShaderProgram* busy;
    extern QOpenGLShaderProgram* bars;
    extern QOpenGLShaderProgram* bbox;
}
