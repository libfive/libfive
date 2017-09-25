#include "gui/shader.hpp"

namespace Shader
{

QOpenGLShaderProgram* flat;
QOpenGLShaderProgram* monochrome;
QOpenGLShaderProgram* shaded;
QOpenGLShaderProgram* busy;
QOpenGLShaderProgram* bars;
QOpenGLShaderProgram* bbox;

void initializeGL()
{
    flat = new QOpenGLShaderProgram;
    flat->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/basic.vert");
    flat->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/flat.frag");
    flat->link();

    monochrome = new QOpenGLShaderProgram;
    monochrome->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/basic.vert");
    monochrome->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/monochrome.frag");
    monochrome->link();

    shaded = new QOpenGLShaderProgram;
    shaded->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/basic.vert");
    shaded->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/shaded.frag");
    shaded->link();

    busy = new QOpenGLShaderProgram;
    busy->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/basic.vert");
    busy->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/busy.frag");
    busy->link();

    bars = new QOpenGLShaderProgram;
    bars->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/basic.vert");
    bars->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/bars.frag");
    bars->link();

    bbox = new QOpenGLShaderProgram;
    bbox->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/bbox.vert");
    bbox->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/bbox.frag");
    bbox->link();
}

}
