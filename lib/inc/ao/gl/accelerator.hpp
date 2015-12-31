#pragma once

#include <string>
#include <unordered_map>

#include <Eigen/Dense>

#include "ao/gl/core.hpp"

class Tree;
class Atom;
class Region;

/*
 *  An Accelerator object speeds up tree rendering with OpenGL
 */
class Accelerator
{
public:
    Accelerator(const Tree* tree);
    ~Accelerator();

    /*
     *  Render a region and return the resulting depth image
     */
    Eigen::ArrayXXd Render(const Region& r);

    /*
     *  Render a target region and blit it into the given depth image
     */
    void Render(const Region& r, Eigen::ArrayXXd& img);

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

    GLuint fbo; // Frame-buffer object
    GLuint tex; // Target texture

    static const std::string vert;
    static const std::string frag;
};
