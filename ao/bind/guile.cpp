#include <libguile.h>
#include <cstdlib>
#include "lib.h"

void init_ao(void*)
{

}

void scm_init_ao()
{
    scm_c_define_module("ao", init_ao, NULL);
}
