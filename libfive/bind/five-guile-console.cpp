#include <libguile.h>
#include "libfive-guile.h"

static void guile_main (void *data, int argc, char *argv[])
{
    static_cast<void>(data); // avoid unused parameter warning

    scm_init_libfive_modules();
    scm_c_use_module("libfive kernel");
    scm_c_use_module("libfive vec");
    scm_c_use_module("libfive csg");
    scm_c_use_module("libfive shapes");
    scm_c_use_module("libfive util");
    scm_c_use_module("libfive sandbox");
    scm_shell (argc, argv);
}

int main (int argc, char *argv[])
{
    scm_boot_guile (argc, argv, guile_main, 0);

    return 0;
}
