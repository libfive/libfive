/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include "libfive/render/brep/object_pool.hpp"
#include "libfive/render/brep/default_new_delete.hpp"

// Though the object pool is templated, it requires explicit instantiation
// to save on compile time, so we pull in the source here.
#include "../src/render/brep/object_pool.inl"

using namespace libfive;

struct Dummy {
    Dummy() { i = 1; }
    void reset() { i = 2; }
    DEFAULT_OPERATORS_NEW_AND_DELETE
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

TEST_CASE("ObjectPool::size") {
    ObjectPool<Dummy> pool;
    auto a = pool.get();
    REQUIRE(pool.size() == 1);

    pool.put(a);
    REQUIRE(pool.size() == 0);

    pool.get();
    REQUIRE(pool.size() == 1);

    pool.get();
    REQUIRE(pool.size() == 2);
}
