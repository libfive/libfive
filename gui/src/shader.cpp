#include "gui/shader.hpp"

namespace Shader
{

QOpenGLShaderProgram* flat;

void initializeGL()
{
    flat = new QOpenGLShaderProgram;
    flat->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/basic.vert");
    flat->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/flat.frag");
    flat->link();
}

}
