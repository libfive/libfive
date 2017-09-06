#pragma once

#include <libguile.h>
#include "ao/ao.h"

#ifdef __cplusplus
extern "C" {
#endif

void scm_init_ao_modules();

bool scm_is_tree(SCM t);
ao_tree scm_to_tree(SCM t);

#ifdef __cplusplus
}
#endif
