#include <libguile.h>

#include "store.h"
#include "fab.h"


void init_fab()
{
    scm_c_define_gsubr("make-num-token", 2, 0, 0, (void*)make_num_token);
    scm_c_define_gsubr("make-var-token", 2, 0, 0, (void*)make_var_token);
    scm_c_define_gsubr("make-op-token", 3, 0, 0, (void*)make_op_token);
    scm_c_define_gsubr("make-store", 0, 0, 0, (void*)make_store);
    scm_c_define_gsubr("describe-store", 1, 0, 0, (void*)describe_store);
}
