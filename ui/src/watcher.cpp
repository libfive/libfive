#include "ao/ui/watcher.hpp"
#include "ao/ui/window.hpp"

efsw::FileWatcher* ScriptWatcher::parent = nullptr;

static std::string join(std::string directory, std::string filename)
{
    if (directory[directory.length() - 1] == '/')
    {
        directory = directory.substr(0, directory.length() - 1);
    }
    return directory + "/" + filename;
}

ScriptWatcher::ScriptWatcher(Window* window, Callback callback,
                             std::string directory, std::string filename)
    : window(window), target(join(directory, filename)), callback(callback)
{
    if (!parent)
    {
        parent = new efsw::FileWatcher();
        parent->watch();
    }

    parent->addWatch(directory, this, false);

    // Trigger the callback once on construction
    callback(target);
}

void ScriptWatcher::handleFileAction(
        efsw::WatchID watchid, const std::string& dir,
        const std::string& filename, efsw::Action action,
        std::string old_filename)
{
    (void)watchid;
    (void)old_filename;

    if (join(dir, filename) == target && action == efsw::Actions::Modified)
    {
        window->clearFile(target);
        callback(target);
    }
}

