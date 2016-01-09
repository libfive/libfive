#pragma once

#include <string>
#include <unordered_map>
#include <array>

#include <Eigen/Dense>
#include <glm/mat4x4.hpp>

#include "ao/gl/core.hpp"
#include "ao/render/heightmap.hpp"

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
     *  This must be called from the main thread with a current OpenGL context
     */
    Accelerator();
    ~Accelerator();

    /*
     *  Prepares to render the given region, allocating memory for the
     *  textures and saving global z bounds to render correctly
     */
    void init(const Region& r, GLuint depth, GLuint norm);

    /*
     *  Executes the queued-up raycast operations on the given tree
     */
    void flush(Tree* tree);
    void flush(Evaluator* eval);

    /*
     *  Saves our generic transform matrix
     */
    void setMatrix(const glm::mat4& m);

    /*
     *  Render a target region, returning a pair of matrices
     *
     *  The accelerator's context must be current when this is called
     */
    std::pair<DepthImage, NormalImage> Render(Tree* t, const Region& r);

    /*
     *  Render a subregion into the active depth and normal textures
     *
     *  init must be called before this function
     */
    void RenderSubregion(const Region& r);

    /*
     *  Fills a subregion into the active depth and normal textures
     *
     *  init must be called before this function
     */
    void FillSubregion(const Region& r);

protected:
    void populateTape(Evaluator* eval);

    GLFWwindow* context;

    GLuint vs;  // Vertex shader
    GLuint fs;  // Fragment shader
    GLuint prog;    // Shader program

    GLuint vbo; // Vertex buffer object
    GLuint vao; // Vertex array object

    GLuint fbo; // Frame-buffer object

    std::array<GLfloat, 12> mat; // Generic transform matrix

    // Vertices of quads to be rendered in bulk
    std::vector<glm::ivec4> data;

    // Instruction tape for the fragment shader
    std::vector<glm::ivec3> tape;

    // Static shader strings
    static const std::string vert;
    static const std::string frag;
};
