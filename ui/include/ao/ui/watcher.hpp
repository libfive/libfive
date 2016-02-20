/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
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
    std::unique_ptr<efsw::FileWatcher> parent;
};
