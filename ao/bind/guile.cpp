#include <libguile.h>
#include <cstdlib>
#include "lib.h"

void del_tree(void* t)      { ao_tree_delete((ao_tree)t); }
SCM make_tree(ao_tree t)    { return scm_from_pointer(t, del_tree); }

void init_ao(void*)
{
    scm_c_eval_string(R"(
(use-modules (system foreign))

(define-wrapped-pointer-type
    tree tree? wrap-tree unwrap-tree
    (lambda (o p)
        (format p "#<tree 0x~x>"
        (pointer-address (unwrap-tree o))))) )");
}

extern "C"
{

void scm_init_ao()
{
    scm_c_define_module("ao", init_ao, NULL);
}

}   // extern C
