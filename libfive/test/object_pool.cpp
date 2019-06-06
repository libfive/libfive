/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include "libfive/render/brep/object_pool.hpp"

using namespace Kernel;

struct Dummy {
    Dummy() { i = 1; }
    void reset() { i = 2; }
    static void* operator new[](std::size_t sz) {
        return ::operator new[](sz);
    }
    void operator delete[](void* ptr) {
        ::operator delete[](ptr);
    }
    static void* operator new(size_t sz) {
        return ::operator new(sz);
    }
    void operator delete(void* ptr) {
        ::operator delete(ptr);
    }
    template <typename... Args>
    static void* operator new(size_t sz, Args... args) {
        return ::operator new(sz);
    }
    template <typename... Args>
    void operator delete(void* ptr, Args... args) {
        ::operator delete(ptr);
    }
    int i;
};

TEST_CASE("ObjectPool::get") {
    ObjectPool<Dummy> pool;
    auto a = pool.get();
    REQUIRE(a != nullptr);
    REQUIRE(a->i == 1);

    auto b = pool.get();
    REQUIRE(b != nullptr);
    REQUIRE(b->i == 1);

    // Force a second block allocation
    for (unsigned i=0; i < 1024; ++i) {
        pool.get();
    }
}

TEST_CASE("ObjectPool::put + get") {
    ObjectPool<Dummy> pool;
    auto a = pool.get();
    REQUIRE(a != nullptr);
    REQUIRE(a->i == 1);

    pool.put(a);
    auto b = pool.get();
    REQUIRE(b != nullptr);
    REQUIRE(b == a);
    REQUIRE(b->i == 2);
}
