/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <future>
#include <vector>

#include <Eigen/Eigen>
#include <Eigen/StdVector>

#include "libfive/render/brep/per_thread_brep.hpp"

namespace libfive {

template <unsigned N>
class BRep
{
public:
    BRep() { verts.push_back(Eigen::Matrix<float, N, 1>::Zero()); }

    /*  Flat array of point positions
     *  The 0th position is reserved as a marker */
    std::vector<Eigen::Matrix<float, N, 1>,
                Eigen::aligned_allocator<Eigen::Matrix<float, N, 1>>> verts;

    /*  [N-1]-dimensional objects (line segments, triangles) */
    std::vector<Eigen::Matrix<uint32_t, N, 1>,
                Eigen::aligned_allocator<Eigen::Matrix<uint32_t, N, 1>>> branes;

    uint32_t pushVertex(const Eigen::Matrix<float, N, 1>& v) {
        uint32_t out = verts.size();
        verts.push_back(v);
        return out;
    }

    /*
     *  Collects a set of PerThreadBRep objects into this BRep.
     *  The children must be a valid set of breps, i.e. generated with
     *  the same atomic index, so that their indices are globally
     *  unique and completely fill the range starting from 1.
     *
     *  If workers is 0, spins up one thread per child, otherwise spins
     *  up the requested number of threads to do the merge in parallel.
     */
    void collect(const std::vector<PerThreadBRep<N>>& children,
                 unsigned workers=0)
    {
        assert(verts.size() == 1);
        assert(branes.size() == 0);

        if (workers == 0) {
            workers = children.size();
        }

        // Build big enough vectors to hold everything, since we're going
        // to be dropping items in through multiple threads.
        size_t num_verts = 1;
        size_t num_branes = 0;
        for (const auto& c : children) {
            num_verts += c.verts.size();
            num_branes += c.branes.size();
        }
        verts.resize(num_verts);
        branes.resize(num_branes);

        std::vector<std::future<void>> futures;
        futures.resize(workers);

        for (unsigned i=0; i < workers; ++i) {
            futures[i] = std::async(std::launch::async,
                [i, workers, this, &children]() {
                    for (unsigned j=i; j < children.size(); j += workers) {
                        const auto& c = children[j];

                        // Unpack vertices, which all have unique indexes into
                        // our collecting vertex array.
                        for (unsigned k=0; k < c.indices.size(); ++k) {
                            verts.at(c.indices.at(k)) = c.verts.at(k);
                        }

                        // Figure out where to position the branes in the
                        // collecting branes array, using a simple offset
                        // from the start.
                        size_t offset = 0;
                        for (unsigned k=0; k < j; ++k) {
                            offset += children[k].branes.size();
                        }

                        // Then save all of the branes
                        for (unsigned k=0; k < c.branes.size(); ++k) {
                            branes[offset + k] = c.branes[k];
                        }
                    }
                }
            );
        }

        for (auto& f : futures) {
            f.wait();
        }
    }

    /*
     * Sorts the triangles of the brep, allowing consistent results regardless
     * of multithreading, provided that there are not duplicate vertices.
     * Asserts if called when there are duplicate vertices.
     */
    void sort() {
        // sortMapForward[i] is the current location of the vertex that
        // should be in the i'th place.
        std::vector<uint32_t> sortMapForward(verts.size());
        assert(sortMapForward.size() == verts.size());
        for (auto i = 0; i < verts.size(); ++i) {
            sortMapForward[i] = i;
        }
        std::sort(sortMapForward.begin(), sortMapForward.end(),
            [this](uint32_t a, uint32_t b) {
            if (a == 0) {
                // Keep our "marker" at the 0th point at the beginning.
                return false;
            }
            else if (b == 0) {
                return true;
            }
            const auto& aVert = verts[a];
            const auto& bVert = verts[b];
            for (auto i = 0; i < N; ++i) {
                if (aVert[i] != bVert[i]) {
                    return aVert[i] < bVert[i];
                };
            }
            // If we reach here, we have a duplicate vertex.
            assert(false);
            return false;
        });
        // sortMapBackward[i] is the proper location of the vertex that
        // is currently in the i'th place.
        std::vector<uint32_t> sortMapBackward(verts.size());
        std::vector<Eigen::Matrix<float, N, 1>,
            Eigen::aligned_allocator<Eigen::Matrix<float, N, 1>>> 
            newVerts(verts.size());

        for (auto i = 0; i < verts.size(); ++i) {
            newVerts[i] = verts[sortMapForward[i]];
            sortMapBackward[sortMapForward[i]] = i;
        }

        verts = newVerts;

        for (auto& brane : branes) {
            for (auto i = 0; i < N; ++i) {
                auto& vert = brane[i];
                assert(vert < verts.size());
                assert(vert != 0);
                vert = sortMapBackward[vert];
            }
        }
        std::sort(branes.begin(), branes.end(), [](auto a, auto b) {
            for (auto i = 0; i < N; ++i) {
                if (a[i] != b[i]) {
                    return a[i] < b[i];
                };
            }
            return false;
        });
    }
};

}   // namespace libfive
