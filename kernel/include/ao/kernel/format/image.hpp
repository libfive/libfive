#pragma once

#include <string>
#include <Eigen/Dense>

#include "ao/kernel/render/heightmap.hpp"

namespace Image
{
    bool SavePng(std::string filename, const DepthImage& img);
}
