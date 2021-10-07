/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#define DEFAULT_OPERATORS_NEW_AND_DELETE \
    /*  Required for ObjectPool, but we're just using the default */\
    /*  operators because there are no alignment requirements.    */\
    static void* operator new[](std::size_t sz) {                   \
        return ::operator new[](sz);                                \
    }                                                               \
    void operator delete[](void* ptr) {                             \
        ::operator delete[](ptr);                                   \
    }                                                               \
    template <typename... Args>                                     \
    void operator delete[](void* ptr, Args... args) {               \
        ::operator delete(ptr, args...);                            \
    }                                                               \
                                                                    \
    /* Non-array versions are also required for ObjectPool by */    \
    /* Visual Studio's compiler. */                                 \
    template <typename... Args>                                     \
    static void* operator new(size_t sz, Args... args) {            \
        return ::operator new(sz, args...);                         \
    }                                                               \
    void operator delete(void* ptr) {                               \
        ::operator delete(ptr);                                     \
    }                                                               \
    template <typename... Args>                                     \
    void operator delete(void* ptr, Args... args) {                 \
        ::operator delete(ptr, args...);                            \
    }

// Eigen 4 makes EIGEN_MAKE_ALIGNED_OPERATOR_NEW a no-op, but... this doesn't
// seem to work (see libfive issues #462 and #464).  This macro replaces it
// for new Eigen versions, while staying backwards-compatible for Eigen 3.x
#if EIGEN_MAJOR_VERSION == 4
#define ALIGNED_OPERATOR_NEW_AND_DELETE(T) \
    void *operator new[](std::size_t size) {                        \
        return std::aligned_alloc(std::alignment_of_v<T>, size);    \
    }                                                               \
                                                                    \
    void operator delete[](void* ptr) {                             \
        ::operator delete[](ptr);                                   \
    }
#else
#define ALIGNED_OPERATOR_NEW_AND_DELETE(T) EIGEN_MAKE_ALIGNED_OPERATOR_NEW
#endif
