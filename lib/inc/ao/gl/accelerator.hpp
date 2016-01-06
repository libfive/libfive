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
    Accelerator(const Tree* tree);
    ~Accelerator();

    /*
     *  Prepares to render the given region, allocating memory for the
     *  textures and saving global z bounds to render correctly
     */
    void init(const Region& r, GLuint depth, GLuint norm);

    /*
     *  Saves our generic transform matrix
     */
    void setMatrix(const glm::mat4& m);

    /*
     *  Render a target region, returning a pair of matrices
     *
     *  The accelerator's context must be current when this is called
     */
    std::pair<DepthImage, NormalImage> Render(const Region& r);

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

    /*
     *  Converts a tree to an OpenGL 3.3 fragment shader
     */
    enum Mode { DEPTH, NORMAL };
    std::string toShader(const Tree* tree);
    std::string toShaderFunc(const Tree* tree, Mode mode);

    /*
     *  Converts a single Atom into a shader string, incrementing index
     *  and storing the atom in atoms
     */
    std::string toShader(const Atom* m, Mode mode);

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

    std::array<GLfloat, 12> mat; // Generic transform matrix

    // Uniform locations are saved here
    GLint xbounds_loc;
    GLint ybounds_loc;
    GLint zbounds_loc;
    GLint nk_loc;

    // Static shader strings
    static const std::string vert;
    static const std::string frag;
};
