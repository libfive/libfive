#pragma once

#include <list>

#include <glm/mat4x4.hpp>

#include "ao/gl/core.hpp"

class Tree;

/*
 *  The Frame class contains and draws many rendered Tree textures
 */
class Frame
{
public:
    /*
     *  Constructor and destructor
     */
    explicit Frame(Tree* tree);
    ~Frame();

    /*
     *  Draws the set of textures with the given matrix applied
     */
    void draw(const glm::mat4& m) const;

    /*
     *  Pushes a new render task to the stack at the given matrix
     */
    void render(const glm::mat4& m, size_t ni, size_t nj);

protected:
    Tree* const tree;

    GLuint vs;  // Vertex shader
    GLuint fs;  // Fragment shader
    GLuint prog;    // Shader program

    GLuint vbo; // Vertex buffer object
    GLuint vao; // Vertex array object

    // List of texture planes and the matrices with which they were rendered
    glm::mat4 m_render;
    GLuint depth;


    // Shader source strings
    static const std::string vert;
    static const std::string frag;
};
