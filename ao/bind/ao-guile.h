#pragma once

#include <libguile.h>
#include "ao/ao.h"

#ifdef __cplusplus
extern "C" {
#endif

struct scm_ao_tree {
    // Tree pointer
    ao_tree t;

    // Textual location of this tree (only used for vars)
    int32_t text_pos[2];

    // Scheme tree indexing for the given tree
    // (only used for vars, terminated with -1)
    int32_t* tree_pos;

    // Value of this tree (only used for vars)
    float value;
};

////////////////////////////////////////////////////////////////////////////////

struct scm_ao_tree;

void scm_init_ao_kernel_module();

bool scm_is_tree(SCM t);
scm_ao_tree* scm_to_tree(SCM t);

#ifdef __cplusplus
}
#endif
