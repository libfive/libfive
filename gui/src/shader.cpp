#include "gui/shader.hpp"

namespace Shader
{

QOpenGLShaderProgram* flat;
QOpenGLShaderProgram* shaded;

void initializeGL()
{
    flat = new QOpenGLShaderProgram;
    flat->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/basic.vert");
    flat->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/flat.frag");
    flat->link();

    shaded = new QOpenGLShaderProgram;
    shaded->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/basic.vert");
    shaded->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/shaded.frag");
    shaded->link();
}

}
