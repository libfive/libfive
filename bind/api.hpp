#pragma once

#include <string>

class Store;
class Token;
class Tree;

extern "C"
{
    Store* store_new();
    void store_delete(Store* ptr);

////////////////////////////////////////////////////////////////////////////////

    Token* token_x(Store* s);
    Token* token_y(Store* s);
    Token* token_z(Store* s);

    Token* token_const(Store* s, float f);

////////////////////////////////////////////////////////////////////////////////

    Tree* tree_new(Store* store, Token* root);
    void tree_delete(Tree* ptr);

    float tree_eval_double(Tree* tree, float x, float y, float z);

    void tree_export_heightmap(Tree* tree, char* filename,
                               float xmin, float xmax,
                               float ymin, float ymax,
                               float zmin, float zmax, float res);

    void tree_export_mesh(Tree* tree, char* filename,
                          float xmin, float xmax,
                          float ymin, float ymax,
                          float zmin, float zmax, float res);

////////////////////////////////////////////////////////////////////////////////

    void window_show_tree(char* filename, char* name, Tree* tree);
    void window_watch_file(std::string dir, std::string file);

    extern void (*window_watch_callback)(const char*);

////////////////////////////////////////////////////////////////////////////////

    void run();

}
