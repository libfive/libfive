/*
 *  Copyright (C) 2016 Matthew Keeter
 *
 *  This file is part of Ao.
 *
 *  Ao is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
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

