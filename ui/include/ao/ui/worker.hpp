#include <chrono>
#include <future>

#include <Eigen/Dense>

#include "ao/ui/gl/core.hpp"
#include "ao/kernel/render/heightmap.hpp"

struct Task;

/*
 *  A worker contains all of the data needed for a running render task
 */
struct Worker
{
    /*
     *  Constructs a CPU worker from the given Tree and a task
     *  (higher task divisors produce lower-resolution workers)
     *
     *  depth and norm are target textures in which results are stored
     */
    Worker(Tree* tree, const Task& task);

    /*
     *  On destruction, join the thread
     */
    ~Worker();

    /*
     *  Returns true if the worker is running, false otherwise
     */
    bool running() const;

    /*
     *  Attempts to halt the task by setting the abort flag
     */
    void halt();

    /*
     *  Polls the worker, loading data into the given textures if complete
     *
     *  Returns RUNNING if the worker is still running, DONE if the worker
     *  is done, and ABORTED if the worker was aborted but is complete.
     *
     *  On success, images are deployed to depth and norm textures
     *  (so there needs to be a current OpenGL context)
     */
    enum State { RUNNING, DONE, ABORTED };
    State poll(GLuint depth, GLuint norm);

    /*  Region that is being analyzed  */
    Region region;

    /*  Fun async stuff  */
    std::promise<std::pair<DepthImage, NormalImage>> promise;
    std::future<std::pair<DepthImage, NormalImage>> future;
    std::atomic_bool abort;
    std::thread thread;

    /*  Records how long the render took  */
    std::chrono::duration<double> elapsed;
};
