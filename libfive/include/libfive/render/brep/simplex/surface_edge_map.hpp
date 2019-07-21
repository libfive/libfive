/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <boost/container/small_vector.hpp>

namespace libfive {

/*  At various points while meshing, we want to associate
 *  a key (which is a pair of subspace vertex indices) with
 *  an index (which is a mesh surface vertex index).
 *
 *  This data structure is a mostly heap-allocated ordered
 *  map implementation, meant for these kinds of small maps. */
template <unsigned Size>
struct SurfaceEdgeMap
{
public:
    using Key = std::pair<uint64_t, uint64_t>;
    using Value = uint64_t;

    void insert(Key key, Value value) {
        assert(value != 0);

        auto pos = std::lower_bound(keys.begin(),
                                    keys.end(), key);

        if (pos == keys.end() || *pos != key) {
            const auto itr = keys.insert(pos, key);
            const auto n = itr - keys.begin();
            values.insert(values.begin() + n, value);
        }
    }

    Value find(Key key) const {
        auto itr = std::lower_bound(keys.begin(),
                                    keys.end(), key);
        if (itr == keys.end() || *itr != key) {
            return 0;
        } else {
            return values[itr - keys.begin()];
        }
    }

    void clear() {
        keys.clear();
        values.clear();
    }

    size_t size() {
        assert(keys.size() == values.size());
        return keys.size();
    }

    Key key(size_t i) {
        return keys.at(i);
    }

    Value value(size_t i) {
        return values.at(i);
    }

protected:
    boost::container::small_vector<Key, Size> keys;
    boost::container::small_vector<Value, Size> values;
};

}   // namespace libfive
