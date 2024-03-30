#include <cmath>
#include <cstdlib>

#include "catch.hpp"

#include "libfive/oracle/oracle_storage.hpp"
#include "libfive/oracle/oracle_clause.hpp"
#include "libfive/tree/tree.hpp"
#include "libfive/render/brep/mesh.hpp"
#include "libfive/render/brep/settings.hpp"
#include "libfive/render/brep/region.hpp"

using namespace libfive;

struct CurvOracle : public libfive::OracleStorage<LIBFIVE_EVAL_ARRAY_SIZE>
{
    double epsilon_ = 1e-6;
    CurvOracle() {}

    void evalInterval(libfive::Interval& out) override
    {
        // Since Curv currently doesn't do interval arithmetic,
        // return a large interval. Which magic number do I choose?
        //
        // mkeeter: If the top-level tree is just this CurvOracle, then
        // returning [-inf, inf] should be fine; however, if you're
        // transforming it further, then I agree that the math could get iffy.
        // You could also return [NaN, NaN], which should cause interval
        // subdivision to always subdivide (down to individual voxels).

        // This gives rounded edges for a large cube (`cube 9999999`).
        //out = {-10000.0, 10000.0};

        // This gives the same results (rounded edges) for a large cube.
        // I'm using infinity because it is "the least magic" alternative
        // and is not overly scale dependent like 10000 is.
        out = { -std::numeric_limits<double>::infinity(),
                 std::numeric_limits<double>::infinity() };

      #if 0
        // This gives the same results (rounded edges) for a large cube.
        out = { std::numeric_limits<double>::quiet_NaN(),
                std::numeric_limits<double>::quiet_NaN() };
      #endif
    }
    void evalPoint(float& out, size_t index=0) override
    {
        const auto pt = points.col(index);
        out = sqrt(pt.x()*pt.x()+pt.y()*pt.y()+pt.z()*pt.z()) - 1.0;
    }
    void checkAmbiguous(Eigen::Block<
        Eigen::Array<bool, 1, LIBFIVE_EVAL_ARRAY_SIZE>,
        1, Eigen::Dynamic>) override
    {
        // Nothing to do here, because we can only find one derivative
        // per point. Points on sharp features may not be handled correctly.
    }
    void evalFeatures(boost::container::small_vector<libfive::Feature, 4>& out)
    override {
        // Find one derivative with partial differences.

        float centre, dx, dy, dz;
        Eigen::Vector3f before = points.col(0);
        evalPoint(centre);

        points.col(0) = before + Eigen::Vector3f(epsilon_, 0.0, 0.0);
        evalPoint(dx);

        points.col(0) = before + Eigen::Vector3f(0.0, epsilon_, 0.0);
        evalPoint(dy);

        points.col(0) = before + Eigen::Vector3f(0.0, 0.0, epsilon_);
        evalPoint(dz);

        points.col(0) = before;

        out.push_back(
            Eigen::Vector3f(dx - centre, dy - centre, dz - centre)
            .normalized());
    }
};
struct CurvOracleClause : public libfive::OracleClause
{
    CurvOracleClause() {}
    std::unique_ptr<libfive::Oracle> getOracle() const override
    {
        return std::unique_ptr<libfive::Oracle>(new CurvOracle());
    }
    std::string name() const override { return "CurvOracleClause"; }
};

// reproduce crash in multithreaded evaluation
TEST_CASE("curv_multithread")
{
    Tree tree(std::unique_ptr<OracleClause>(new CurvOracleClause()));
    BRepSettings settings;
    settings.workers = 0; /* crash if workers != 1 */
    settings.min_feature = 2.0/25.0;
    Region<3> region
        ({-1.0 - settings.min_feature,
          -1.0 - settings.min_feature,
          -1.0 - settings.min_feature},
         {+1.0 + settings.min_feature,
          +1.0 + settings.min_feature,
          +1.0 + settings.min_feature});

    auto mesh = Mesh::render(tree, region, settings);
    REQUIRE(mesh != nullptr);
}
