#pragma once

#include <string>
#include <unordered_map>

#include <Eigen/Dense>
#include <glm/mat4x4.hpp>

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
    /*
     *  Constructs a new Accelerator
     *
     *  This must be called from the main thread
     */
    Accelerator(const Tree* tree);
    ~Accelerator();

    /*
     *  Saves our generic transform matrix
     */
    void setMatrix(const glm::mat4& m);

    /*
     *  Makes this accelerator's context current
     */
    void makeContextCurrent() const;

    /*
     *  Render a region and return the resulting depth image
     *
     *  The accelerator's context must be current when this is called
     */
    Eigen::ArrayXXd Render(const Region& r);

    /*
     *  Render a target region and blit it into the given depth image
     *
     *  The accelerator's context must be current when this is called
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

    GLFWwindow* context;

    GLuint vs;  // Vertex shader
    GLuint fs;  // Fragment shader
    GLuint prog;    // Shader program

    GLuint vbo; // Vertex buffer object
    GLuint vao; // Vertex array object

    GLuint fbo; // Frame-buffer object
    GLuint tex; // Target texture

    GLfloat mat[12]; // Generic transform matrix

    // Static shader strings
    static const std::string vert;
    static const std::string frag;
};
