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
#include <iostream>
#include <string>
#include <fstream>

#include "ao/kernel/tree/store.hpp"
#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/tree/opcode.hpp"
#include "ao/kernel/tree/token.hpp"

#include "ao/kernel/eval/evaluator.hpp"

#include "ao/kernel/render/heightmap.hpp"
#include "ao/kernel/render/dc.hpp"
#include "ao/kernel/format/image.hpp"
#include "ao/kernel/format/mesh.hpp"

#include "ao/ui/window.hpp"
#include "ao/ui/watcher.hpp"

#include "api.hpp"

void store_delete(Store* ptr)
{
    delete ptr;
}

Store* store_new()
{
    return new Store();
}

////////////////////////////////////////////////////////////////////////////////

Token* token_x(Store* s) { return s->X(); }
Token* token_y(Store* s) { return s->Y(); }
Token* token_z(Store* s) { return s->Z(); }

Token* token_const(Store* s, float f)   { return s->constant(f); }

////////////////////////////////////////////////////////////////////////////////

void tree_delete(Tree* ptr)
{
    delete ptr;
}

Tree* tree_new(Store* store, Token* root)
{
    return new Tree(store, root);
}

float tree_eval_double(Tree* tree, float x, float y, float z)
{
    auto e = Evaluator(tree);
    return e.eval(x, y, z);
}

void tree_export_heightmap(Tree* tree, char* filename,
                           float xmin, float xmax,
                           float ymin, float ymax,
                           float zmin, float zmax, float res)
{
    auto f = std::string(filename);
    assert(f.substr(f.length() - 4, 4) == ".png");

    Region region({xmin, xmax}, {ymin, ymax}, {zmin, zmax}, res);
    std::atomic_bool abort(false);

    auto img = Heightmap::Render(tree, region, abort);

    Image::SavePng(f, img.first);
}

void tree_export_mesh(Tree* tree, char* filename,
                      float xmin, float xmax,
                      float ymin, float ymax,
                      float zmin, float zmax, float res)
{
    auto f = std::string(filename);
    assert(f.substr(f.length() - 4, 4) == ".stl");

    Region region({xmin, xmax}, {ymin, ymax}, {zmin, zmax}, res);
    std::atomic_bool abort(false);

    auto mesh = DC::Render(tree, region);
    mesh.writeSTL(f);
}

////////////////////////////////////////////////////////////////////////////////

// window_callback will be initialized by a language-specific init function
void (*window_watch_callback)(const char*) = nullptr;

static void window_watch_callback_(std::string s)
{
    window_watch_callback(s.c_str());
}

void window_show_tree(char* filename, char* name, Tree* tree)
{
    Window::instance()->addTree(filename, name, tree);
}

void window_watch_file(std::string dir, std::string file)
{
    new ScriptWatcher(Window::instance(), window_watch_callback_,
                      dir, file);
}

////////////////////////////////////////////////////////////////////////////////

void ao_run()
{
    auto window = Window::instance();
    window->draw();
    window->run();
}

void ao_halt()
{
    Window::instance()->quit();
}
