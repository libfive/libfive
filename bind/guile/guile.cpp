#include <libguile.h>

#include "../api.hpp"

extern "C"
{
    void ao_init_guile();
}

static void* load(void* data)
{
    scm_c_primitive_load(static_cast<char*>(data));
    return nullptr;
}

static void callback(const char* filename)
{
    scm_with_guile(load, (void*)filename);
}

void ao_init_guile()
{
    window_watch_callback = callback;
}
