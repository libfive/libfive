#pragma once

#include <functional>
#include <string>

#include <efsw/include/efsw/efsw.hpp>

class Window;

class ScriptWatcher : public efsw::FileWatchListener
{
public:
    /*  Callback functions takes a single string, which is the filename  */
    typedef std::function<void(std::string)> Callback;

    /*
     *  Construct a new ScriptWatcher for the given file
     *
     *  The callback is triggered once in this constructor
     */
    ScriptWatcher(Window* window, Callback trigger,
                  std::string directory, std::string filename);

protected:
    /*
     *  Overloaded function from efsw::FileWatchListener
     */
    void handleFileAction(efsw::WatchID watchid, const std::string& dir,
                          const std::string& filename, efsw::Action action,
                          std::string old_filename="") override;

    /*  The target window (used to clear frames on file change)  */
    Window* const window;

    /*  Target filename  */
    const std::string target;

    /*  Callback invoked on file changes  */
    Callback callback;

    /*  Global FileWatcher instance  */
    static efsw::FileWatcher* parent;
};
