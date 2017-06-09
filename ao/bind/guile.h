#pragma once

#include <libguile.h>
#include "lib.h"

#ifdef __cplusplus
extern "C" {
#endif

void scm_init_ao_kernel_module();

bool scm_is_tree(SCM t);
ao_tree scm_to_tree(SCM t);

#ifdef __cplusplus
}
#endif
