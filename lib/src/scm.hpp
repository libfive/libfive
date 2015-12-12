#pragma once

#include <libguile.h>

extern "C" {
    /*
     *  Helper functions to manipulate store pointers
     */
    void store_delete(void* ptr);
    SCM store_new();

    /*
     *  Helper functions to manipulate tree pointers
     */
    SCM tree_new(SCM store, SCM root);
    void tree_delete(void* ptr);

    /*
     *  Tree evaluation functions
     */
    SCM tree_eval_double(SCM tree, SCM x, SCM y, SCM z);
    SCM tree_eval_interval(SCM tree, SCM x, SCM y, SCM z);
    SCM tree_eval_doubles(SCM tree, SCM x, SCM y, SCM z);
    SCM tree_eval_intervals(SCM tree, SCM x, SCM y, SCM z);
    SCM tree_mode_double(SCM tree, SCM count);
    SCM tree_mode_interval(SCM tree, SCM count);

    /*
     *  Return variables wrapped as SCM pointers
     */
    SCM token_x(SCM store);
    SCM token_y(SCM store);
    SCM token_z(SCM store);

    /*
     *  Make a token for a constant
     */
    SCM token_const(SCM store, SCM n);

    /*
     *  Make a token for an operation
     */
    SCM token_op(SCM store, SCM op, SCM args);

    /*
     *  Initialize the shared library functions
     */
    void libao_init();
}
