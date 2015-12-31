#pragma once

#include <string>
#include <unordered_map>

#include "ao/gl/core.hpp"

class Tree;
class Atom;

/*
 *  An Accel object speeds up tree rendering with OpenGL
 */
class Accel
{
public:
    Accel(const Tree* tree);
    ~Accel();

    /*
     *  Converts a tree to an OpenGL 3.3 fragment shader
     */
    std::string toShader(const Tree* tree);

    /*
     *  Converts a single Atom into a shader string, incrementing index
     *  and storing the atom in atoms
     */
    std::string toShader(const Atom* a);

protected:
    std::unordered_map<const Atom*, size_t> atoms;
    size_t index=0;

    GLuint vs;  // Vertex shader
    GLuint fs;  // Fragment shader
    GLuint prog;    // Shader program

    GLuint vbo; // Vertex buffer object
    GLuint vao; // Vertex array object

    static const std::string vert;
};
