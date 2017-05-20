#pragma once

#include <string>
#include <Eigen/Dense>

namespace Kernel {

typedef Eigen::Array<float, Eigen::Dynamic, Eigen::Dynamic> DepthImage;
typedef Eigen::Array<uint32_t, Eigen::Dynamic, Eigen::Dynamic> NormalImage;

namespace Image
{
    bool SavePng(std::string filename, const DepthImage& img);
}

}   // namespace Kernel
