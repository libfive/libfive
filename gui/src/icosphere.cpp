/*
Studio: a simple GUI for the Ao CAD kernel
Copyright (C) 2017  Matt Keeter

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
#include <QColor>
#include <Eigen/StdVector>
#include <boost/math/constants/constants.hpp>

#include "gui/icosphere.hpp"
#include "gui/shader.hpp"

Icosphere::Icosphere()
    : vert_vbo(QOpenGLBuffer::VertexBuffer),
      tri_vbo(QOpenGLBuffer::IndexBuffer)
{
    // Nothing to do here
}

void Icosphere::initializeGL(int subdiv)
{
    initializeOpenGLFunctions();

    auto p = boost::math::constants::phi<float>();
    std::vector<Eigen::Vector3f,
                Eigen::aligned_allocator<Eigen::Vector3f>> vs = {
       {-1,  0,  p},
       { 1,  0,  p},
       {-1,  0, -p},
       { 1,  0, -p},
       { 0,  p,  1},
       { 0,  p, -1},
       { 0, -p,  1},
       { 0, -p, -1},
       { p,  1,  0},
       {-p,  1,  0},
       { p, -1,  0},
       {-p, -1,  0},
    };

    std::vector<Eigen::Vector3i,
                Eigen::aligned_allocator<Eigen::Vector3i>> ts = {
       {0,  4,  1},
       {0,  9,  4},
       {9,  5,  4},
       {4,  5,  8},
       {4,  8,  1},
       {8,  10, 1},
       {8,  3,  10},
       {5,  3,  8},
       {5,  2,  3},
       {2,  7,  3},
       {7,  10, 3},
       {7,  6,  10},
       {7,  11, 6},
       {11, 0,  6},
       {0,  1,  6},
       {6,  1,  10},
       {9,  0,  11},
       {9,  11, 2},
       {9,  2,  5},
       {7,  2,  11}
    };

    // Map from edges (in normalized order) to vertices
    // placed at the center of that edge
    std::map<std::pair<int, int>, int> es;

    // Helper function to get a vertex from an edge pair
    auto edge = [&](int a, int b) {
        auto key = std::make_pair(std::min(a, b), std::max(a, b));
        auto itr = es.find(key);
        if (itr == es.end())
        {
            itr = es.insert({key, vs.size()}).first;
            vs.push_back((vs[a] + vs[b]) / 2);
        }
        return itr->second;
    };

    for (int i=0; i < subdiv; ++i)
    {
        decltype(ts) new_ts;
        for (const auto& t : ts)
        {
            new_ts.push_back({t(0), edge(t(0), t(1)), edge(t(0), t(2))});
            new_ts.push_back({t(1), edge(t(1), t(2)), edge(t(1), t(0))});
            new_ts.push_back({t(2), edge(t(2), t(0)), edge(t(2), t(1))});
            new_ts.push_back({edge(t(0), t(1)),
                              edge(t(1), t(2)),
                              edge(t(2), t(0))});
        }
        ts = new_ts;
    }
    tri_count = ts.size();

    // Project all of the points onto the unit sphere
    for (auto& v : vs)
    {
        v.normalize();
    }

    vao.create();
    vao.bind();

    tri_vbo.create();
    tri_vbo.setUsagePattern(QOpenGLBuffer::StaticDraw);
    tri_vbo.bind();
    tri_vbo.allocate(ts.data(), ts.size() * sizeof(*ts.data()));

    vert_vbo.create();
    vert_vbo.setUsagePattern(QOpenGLBuffer::StaticDraw);
    vert_vbo.bind();
    vert_vbo.allocate(vs.data(), vs.size() * sizeof(*vs.data()));

    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3*sizeof(GLfloat), NULL);
    glEnableVertexAttribArray(0);

    tri_vbo.release();
    vert_vbo.release();
    vao.release();
}

void Icosphere::draw(QMatrix4x4 M, QVector3D pos, float r, QColor color)
{
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    M.translate(pos);
    M.scale(r);

    Shader::point->bind();
    glUniformMatrix4fv(Shader::point->uniformLocation("M"), 1, GL_FALSE, M.data());
    glUniform4f(Shader::point->uniformLocation("frag_color"),
                color.redF(), color.greenF(), color.blueF(), 0.2f);

    glDisable(GL_DEPTH_TEST);
    vao.bind();
    tri_vbo.bind();
    glDrawElements(GL_TRIANGLES, tri_count * 3, GL_UNSIGNED_INT, NULL);

    glDisable(GL_BLEND);

    glEnable(GL_DEPTH_TEST);
    glUniform4f(Shader::point->uniformLocation("frag_color"),
                color.redF(), color.greenF(), color.blueF(), 0.8f);
    glDrawElements(GL_TRIANGLES, tri_count * 3, GL_UNSIGNED_INT, NULL);

    vao.release();
    Shader::point->release();
}
