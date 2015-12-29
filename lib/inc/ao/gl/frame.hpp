#pragma once

#include <list>
#include <future>

#include <glm/mat4x4.hpp>
#include <Eigen/Dense>

#include "ao/gl/core.hpp"
#include "ao/render/heightmap.hpp"

class Tree;

/*
 *  The Frame class contains and draws many rendered Tree textures
 */
class Frame
{
public:
    /*
     *  Constructor and destructor
     *
     *  On construction, takes ownership of the given Tree and sets
     *  its parent pointer.
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
    void render(const glm::mat4& m, size_t ni, size_t nj, size_t nk);

    /*
     *  Check the future watcher and see if it's ready
     *  Returns true if the parent window should redraw
     */
    bool poll();

    /*
     *  Returns true if the render thread is still running
     */
    bool running() const;

protected:
    /*
     *  Kicks off an async render task if there is a pending valid task
     *
     *  Requires the future to be invalid when called
     */
    void startRender();

    /*
     *  Represents a render task
     */
    struct Task
    {
        /* Constructors */
        Task() : ni(0), nj(0), nk(0), level(0) {}
        Task(const glm::mat4& m, size_t ni, size_t nj, size_t nk, int level)
            : mat(m), ni(ni), nj(nj), nk(nk), level(level) {}

        /* Mark the task as invalid */
        void reset() { level = 0; }

        /* Check if the task is valid */
        bool valid() const { return level > 0; }

        /* Transform matrix associated with the task */
        glm::mat4 mat;

        /* Voxel size associated with the task */
        size_t ni, nj, nk;

        /* Subdivision level (1 is highest resolution) */
        size_t level;
    };

    Tree* const tree;

    GLuint vs;  // Vertex shader
    GLuint fs;  // Fragment shader
    GLuint prog;    // Shader program

    GLuint vbo; // Vertex buffer object
    GLuint vao; // Vertex array object

    GLuint depth;  // Depth texture
    GLuint norm;   // Normals texture

    // Represents the current render task
    Task current;

    // Active render task
    Task pending;
    std::promise<std::pair<Eigen::ArrayXXd, Image>> promise;
    std::future<std::pair<Eigen::ArrayXXd, Image>> future;
    std::thread thread;

    // Next matrix to render
    Task next;

    // Shader source strings
    static const std::string vert;
    static const std::string frag;
};
