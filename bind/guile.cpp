#include <iostream>
#include <string>
#include <fstream>

#include <boost/algorithm/string.hpp>
#include <libguile.h>

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

////////////////////////////////////////////////////////////////////////////////

static std::string scm_to_std_string(SCM s)
{
    auto ptr = scm_to_locale_string(s);
    std::string out(ptr);
    free(ptr);

    return out;
}

////////////////////////////////////////////////////////////////////////////////

static Opcode symbol_to_opcode(SCM sym)
{
    auto str = scm_to_std_string(scm_symbol_to_string(sym));

    if (str == "add")           return OP_ADD;
    else if (str == "mul")      return OP_MUL;
    else if (str == "min")      return OP_MIN;
    else if (str == "max")      return OP_MAX;
    else if (str == "sub")      return OP_SUB;
    else if (str == "div")      return OP_DIV;
    else if (str == "atan2")    return OP_ATAN2;
    else if (str == "mod")      return OP_MOD;

    else if (str == "square")   return OP_SQUARE;
    else if (str == "sqrt")     return OP_SQRT;
    else if (str == "abs")      return OP_ABS;
    else if (str == "neg")      return OP_NEG;
    else if (str == "sin")      return OP_SIN;
    else if (str == "cos")      return OP_COS;
    else if (str == "tan")      return OP_TAN;
    else if (str == "asin")     return OP_ASIN;
    else if (str == "acos")     return OP_ACOS;
    else if (str == "atan")     return OP_ATAN;
    else if (str == "exp")      return OP_EXP;

    else                        return INVALID;
}

////////////////////////////////////////////////////////////////////////////////

template <class T>
std::string class_name();

template <>
std::string class_name<Tree>() { return "Tree"; }

template <>
std::string class_name<Token>() { return "Token"; }

template <>
std::string class_name<Store>() { return "Store"; }

template <class T>
static SCM tag_ptr(T* p, void (*finalizer)(void*)=NULL)
{
    return scm_cons(scm_string_to_symbol(
                        scm_from_locale_string(class_name<T>().c_str())),
                    scm_from_pointer(p, finalizer));
}

template <class T>
static T* untag_ptr(SCM ptr)
{
    auto str = scm_to_std_string(scm_symbol_to_string(scm_car(ptr)));

    if (str != class_name<T>())
    {
        scm_wrong_type_arg("untag_ptr", 1, ptr);
    }

    return static_cast<T*>(scm_to_pointer(scm_cdr(ptr)));
}

////////////////////////////////////////////////////////////////////////////////

static void store_delete(void* ptr)
{
    delete static_cast<Store*>(ptr);
}

static SCM store_new()
{
    return tag_ptr(new Store, store_delete);
}

////////////////////////////////////////////////////////////////////////////////

static SCM token_x(SCM s)
{
    return tag_ptr(untag_ptr<Store>(s)->X());
}

static SCM token_y(SCM s)
{
    return tag_ptr(untag_ptr<Store>(s)->Y());
}

static SCM token_z(SCM s)
{
    return tag_ptr(untag_ptr<Store>(s)->Z());
}

////////////////////////////////////////////////////////////////////////////////

static SCM token_const(SCM store, SCM v)
{
    auto s = untag_ptr<Store>(store);
    double value = scm_to_double(v);
    return tag_ptr(s->constant(value));
}

static SCM token_op(SCM store, SCM op_sym, SCM args)
{
    auto s = untag_ptr<Store>(store);
    Opcode op = symbol_to_opcode(op_sym);

    if (op == INVALID)
    {
        scm_wrong_type_arg("make_op_token", 2, op_sym);
        return NULL;
    }

    const int arg_count = Token::args(op);
    if (arg_count != scm_to_int(scm_length(args)))
    {
        scm_misc_error("make_op_token", "Invalid argument count", SCM_EOL);
    }

    Token* a = (arg_count < 1) ? NULL :
        untag_ptr<Token>(scm_car(args));
    Token* b = (arg_count < 2) ? NULL :
        untag_ptr<Token>(scm_cadr(args));

    return tag_ptr(s->operation(op, a, b));
}

////////////////////////////////////////////////////////////////////////////////

static void tree_delete(void* ptr)
{
    auto t = static_cast<Tree*>(ptr);
    if (t->parent == nullptr)
    {
        delete t;
    }
}

static SCM tree_new(SCM store, SCM root)
{
    auto s = untag_ptr<Store>(store);
    auto r = untag_ptr<Token>(root);
    return tag_ptr(new Tree(s, r), tree_delete);
}

static SCM tree_eval_double(SCM tree, SCM x, SCM y, SCM z)
{
    auto t = untag_ptr<Tree>(tree);
    auto e = Evaluator(t);
    double out = e.eval(scm_to_double(x),
                        scm_to_double(y),
                        scm_to_double(z));
    return scm_from_double(out);
}

static SCM tree_eval_interval(SCM tree, SCM x, SCM y, SCM z)
{
    auto t = untag_ptr<Tree>(tree);
    Interval X(scm_to_double(scm_car(x)), scm_to_double(scm_cdr(x)));
    Interval Y(scm_to_double(scm_car(y)), scm_to_double(scm_cdr(y)));
    Interval Z(scm_to_double(scm_car(z)), scm_to_double(scm_cdr(z)));

    auto e = Evaluator(t);
    Interval out = e.eval(X, Y, Z);
    return scm_cons(scm_from_double(out.lower()),
                    scm_from_double(out.upper()));
}

static SCM tree_export_heightmap(SCM tree, SCM filename,
                                 SCM x, SCM y, SCM z, SCM res)
{
    auto t = untag_ptr<Tree>(tree);
    auto f = scm_to_std_string(filename);

    if (f.substr(f.length() - 4, 4) != ".png")
    {
        scm_misc_error("tree_export_heightmap",
                       "Filename must end in '.png'", SCM_EOL);
    }

    Interval X(scm_to_double(scm_car(x)), scm_to_double(scm_cdr(x)));
    Interval Y(scm_to_double(scm_car(y)), scm_to_double(scm_cdr(y)));
    Interval Z(scm_to_double(scm_car(z)), scm_to_double(scm_cdr(z)));

    Region r(X, Y, Z, scm_to_double(res));

    std::atomic_bool abort(false);
    auto img = Heightmap::Render(t, r, abort);

    Image::SavePng(f, img.first);

    return SCM_BOOL_T;
}

static SCM tree_export_mesh(SCM tree, SCM filename,
                            SCM x, SCM y, SCM z, SCM res)
{
    auto t = untag_ptr<Tree>(tree);
    auto f = scm_to_std_string(filename);

    if (f.substr(f.length() - 4, 4) != ".stl")
    {
        scm_misc_error("tree_export_mesh",
                       "Filename must end in '.stl'", SCM_EOL);
    }

    Interval X(scm_to_double(scm_car(x)), scm_to_double(scm_cdr(x)));
    Interval Y(scm_to_double(scm_car(y)), scm_to_double(scm_cdr(y)));
    Interval Z(scm_to_double(scm_car(z)), scm_to_double(scm_cdr(z)));

    Region r(X, Y, Z, scm_to_double(res));

    std::atomic_bool abort(false);
    auto mesh = DC::Render(t, r);
    mesh.writeSTL(f);

    return SCM_BOOL_T;

}

////////////////////////////////////////////////////////////////////////////////

static SCM show_tree(SCM filename, SCM name, SCM tree)
{
    Window::instance()->addTree(scm_to_std_string(filename),
                                scm_to_std_string(name),
                                untag_ptr<Tree>(tree));
    return SCM_ELISP_NIL;
}

static SCM window_clear()
{
    Window::instance()->clearFrames();
    return SCM_ELISP_NIL;
}

////////////////////////////////////////////////////////////////////////////////

static void* watch_load(void* data)
{
    scm_c_primitive_load(static_cast<char*>(data));
    return nullptr;
}

void watch_callback(std::string filename)
{
    scm_with_guile(watch_load, (void*)filename.c_str());
}

static SCM watch_file(SCM dir, SCM file)
{
    std::string dirname = scm_to_std_string(dir);
    std::string filename = scm_to_std_string(file);

    new ScriptWatcher(Window::instance(), watch_callback,
                      dirname, filename);

    return SCM_ELISP_NIL;
}

////////////////////////////////////////////////////////////////////////////////

static void populate_ao_lib(void* data)
{
    (void)data;

    auto define = [](std::string name, int argc, void* func){
        SCM f = scm_c_make_gsubr(name.c_str(), argc, 0, 0, func);
        scm_c_define(name.c_str(), f);
        scm_c_export(name.c_str(), 0); };

    define("store_new", 0, (void*)store_new);

    define("tree_new", 2, (void*)tree_new);
    define("tree_eval_double", 4, (void*)tree_eval_double);
    define("tree_eval_interval", 4, (void*)tree_eval_interval);
    define("tree_export_heightmap", 6, (void*)tree_export_heightmap);
    define("tree_export_mesh", 6, (void*)tree_export_mesh);

    define("token_x", 1, (void*)token_x);
    define("token_y", 1, (void*)token_y);
    define("token_z", 1, (void*)token_z);

    define("token_const", 2, (void*)token_const);
    define("token_op", 3, (void*)token_op);

    define("show_tree", 3, (void*)show_tree);
    define("watch_file", 2, (void*)watch_file);
    define("clear_frames", 0, (void*)window_clear);
}

static void* guile_init(void* data)
{
    (void)data;

    scm_c_define_module("ao lib", populate_ao_lib, nullptr);
    scm_primitive_load(scm_from_locale_string("ao/startup.scm"));

    return nullptr;
}

int main(int argc, char* argv[])
{
    // Initialize OpenGL and trigger an initial draw action
    auto window = Window::instance();
    window->draw();

    // Start a Guile REPL running in a secondary thread
    auto repl = std::thread([=](){
        scm_with_guile(&guile_init, NULL);
        scm_shell(argc, argv); });
    repl.detach();

    window->run();

    return 0;
}
