#pragma once

#include <string>
#include <Eigen/Dense>

namespace Image
{
    bool SavePng(std::string filename, const Eigen::ArrayXXd& img);
}
