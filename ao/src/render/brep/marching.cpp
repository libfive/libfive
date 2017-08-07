#include <array>
#include <set>

#include <Eigen/Eigen>

////////////////////////////////////////////////////////////////////////////////

/*  Compile-time power */
static constexpr int _pow(unsigned P, unsigned N)
{ return (N == 0) ? 1 : P * _pow(P, N - 1); }

/*  Returns the number of vertices in an N-dimensional cube */
static constexpr int _verts(unsigned N)
{ return _pow(2, N - 1); }

/*  Returns the number of edges in an N-dimensional cube */
static constexpr int _edges(unsigned N)
{ return (N == 1) ? 0 : _edges(N - 1) * 2 + _verts(N - 1); }

////////////////////////////////////////////////////////////////////////////////

/*  Represents an edge as a corner-to-corner mapping */
typedef std::pair<int, int> Edge;

/*  Represents the set of edges that define a particular patch
 *  There may not be _edges(N) edges for a particular patch;
 *  use -1 to terminate the array  */
template <unsigned N>
using PatchEdges = std::array<Edge, _edges(N)>;

/*  Represents a full set of patches
 *  Use an empty patch (-1) to terminate */
template <unsigned N>
using Patches = std::array<PatchEdges<N>, _pow(2, N - 1)>;

/*  Represents a full Marching cubes or squares table  */
template <unsigned N>
using MarchingTable = std::array<Patches<N>, _pow(2, _verts(N))>;

////////////////////////////////////////////////////////////////////////////////

/*
 *  Returns a set of all the rigid rotations for a particular dimension
 *  (must be specialized to a particular dimension)
 */
template <unsigned N>
static std::list<Eigen::Matrix<double, N, N>> rigidRotations();

template <>
std::list<Eigen::Matrix<double, 2, 2>> rigidRotations<2>()
{
    Eigen::Matrix2d r;
    r = Eigen::Rotation2Dd(M_PI/2);
    return {r};
}

template <>
std::list<Eigen::Matrix<double, 3, 3>> rigidRotations<3>()
{
    Eigen::Matrix3d x, y, z;
    x = Eigen::AngleAxisd(M_PI/2, Eigen::Vector3d::UnitX());
    y = Eigen::AngleAxisd(M_PI/2, Eigen::Vector3d::UnitY());
    z = Eigen::AngleAxisd(M_PI/2, Eigen::Vector3d::UnitZ());

    return {x, y, z};
}

////////////////////////////////////////////////////////////////////////////////

/*
 *  Load initial cases into a marching squares / cubes table
 *  (must be specialized to a particular dimension)
 */
template <unsigned N>
static void loadCases(MarchingTable<N>& t);

/*
 *  We use the following vertex numbering scheme:
 *      2-------3
 *      |       |
 *      |       |
 *      0-------1
 *
 *      ^ Y
 *      |
 *      ---> X
 */
template <>
void loadCases<2>(MarchingTable<2>& t)
{
    // Empty
    t[0][0][0].first = -1;

    // Single corner
    t[1][0][0] = {0, 1};
    t[1][0][1] = {0, 2};

    // Adjacent corners
    t[3][0][0] = {1, 3};
    t[3][0][1] = {0, 2};

    // Opposite corners
    t[9][0][0] = {0, 1};
    t[9][0][1] = {0, 2};
    t[9][1][0] = {3, 2};
    t[9][1][1] = {3, 1};
}

/*
 *  Based on Figure 5 in Nielsen's Dual Marching Cubes
 *
 *  Vertices are numbered as follows:
 *
 *          6 -------- 7
 *         /          /       Z
 *        / |        / |      ^  _ Y
 *       4----------5  |      | /
 *       |  |       |  |      |/
 *       |  2-------|--3      ---> X
 *       | /        | /
 *       |/         |/
 *       0----------1
 */
template <>
void loadCases<3>(MarchingTable<3>& t)
{
    unsigned bitmap;
    unsigned patch;
    unsigned edge;

    auto begin = [&](std::set<unsigned> verts)
    {
        bitmap = 0;
        for (auto& i : verts)
        {
            bitmap |= 1 << i;
        }
        assert(bitmap > 0 && bitmap < t.size());
        patch = 0;
        edge = 0;
    };

    auto push = [&](Edge e)
    {
        assert(patch < t[bitmap].size());
        assert(edge < t[bitmap][patch].size());
        t[bitmap][patch][edge++] = e;
    };
    auto next = [&]()
    {
        patch++;
        edge = 0;
    };

    // Case 0 (no vertices set)
    // (Nothing to do here)

    // Case 1
    begin({0});
    push({0, 1});
    push({0, 2});
    push({0, 4});

    // Case 2
    begin({0, 1});
    push({1, 3});
    push({0, 2});
    push({0, 4});
    push({1, 5});

    // Case 3
    begin({0, 5});
    push({0, 1});
    push({0, 2});
    push({0, 4});

    next();
    push({5, 7});
    push({5, 4});
    push({5, 1});

    // Case 4
    begin({0, 7});
    push({0, 1});
    push({0, 2});
    push({0, 4});

    next();
    push({7, 6});
    push({7, 3});
    push({7, 5});

    // Case 5
    begin({1, 2, 3});
    push({1, 0});
    push({1, 5});
    push({3, 7});
    push({2, 6});
    push({2, 0});

    // Case 6
    begin({0, 1, 7});
    push({1, 3});
    push({0, 2});
    push({0, 4});
    push({1, 5});

    next();
    push({7, 6});
    push({7, 3});
    push({7, 5});

    // Case 7
    begin({1, 4, 7});
    push({1, 3});
    push({1, 0});
    push({1, 5});

    next();
    push({4, 6});
    push({4, 5});
    push({4, 0});

    next();
    push({7, 6});
    push({7, 3});
    push({7, 5});

    // Case 8
    begin({0, 1, 2, 3});
    push({0, 4});
    push({1, 5});
    push({3, 7});
    push({2, 6});

    // Case 9
    begin({0, 2, 3, 6});
    push({0, 1});
    push({3, 1});
    push({3, 7});
    push({6, 7});
    push({6, 4});
    push({0, 4});

    // Case 10
    begin({0, 4, 3, 7});
    push({0, 1});
    push({0, 2});
    push({4, 6});
    push({4, 5});

    next();
    push({3, 2});
    push({3, 1});
    push({7, 5});
    push({7, 6});

    // Case 11
    begin({0, 2, 3, 7});
    push({0, 1});
    push({3, 1});
    push({7, 5});
    push({7, 6});
    push({2, 6});
    push({0, 4});

    // Case 12
    begin({1, 2, 3, 4});
    push({4, 6});
    push({4, 5});
    push({4, 0});

    next();
    push({1, 0});
    push({1, 5});
    push({3, 7});
    push({2, 6});
    push({2, 0});

    // Case 13
    begin({0, 3, 6, 1});
    push({0, 2});
    push({0, 4});
    push({0, 1});

    next();
    push({3, 7});
    push({3, 2});
    push({3, 1});

    next();
    push({6, 4});
    push({6, 2});
    push({6, 7});

    next();
    push({5, 7});
    push({5, 1});
    push({5, 4});

    // Case 14
    begin({1, 3, 2, 6});
    push({1, 5});
    push({3, 7});
    push({6, 7});
    push({6, 4});
    push({2, 0});
    push({1, 0});

    // Case 15
    begin({0, 2, 3,5, 6});
    push({0, 1});
    push({3, 1});
    push({3, 7});
    push({6, 7});
    push({6, 4});
    push({0, 4});

    next();
    push({5, 7});
    push({5, 1});
    push({5, 4});

    // Case 16
    begin({2, 3, 4, 5, 6});
    push({3,1});
    push({3,7});
    push({6, 7});
    push({5, 7});
    push({5, 1});
    push({4, 0});
    push({2, 0});

    // Case 17
    begin({0, 4, 5, 7});
    push({0, 2});
    push({6, 2});
    push({7, 3});
    push({5, 1});
    push({0, 1});

    // Case 18
    begin({1, 2, 3, 4, 5, 6});
    push({1, 0});
    push({4, 0});
    push({2, 0});

    next();
    push({6, 7});
    push({5, 7});
    push({3, 7});

    // Case 19
    begin({1, 2, 3, 4, 6, 7});
    push({1, 0});
    push({1, 5});
    push({7, 5});
    push({4, 5});
    push({4, 0});
    push({2, 0});

    // Case 20
    begin({2, 3, 4, 5, 6, 7});
    push({4, 0});
    push({5, 1});
    push({3, 1});
    push({2, 0});

    // Case 21
    begin({1,2,3,4,5,6,7});
    push({1, 0});
    push({4, 0});
    push({2, 0});

    // Case 22
    // (nothing to do here)
}

////////////////////////////////////////////////////////////////////////////////

/*
 *  Applies a rigid-body rotation to a bitmasked vertex,
 *  returning a new bitmasked vertex.
 */
template <unsigned N>
static uint8_t applyRotation(uint8_t vert, Eigen::Matrix<double, N, N> rot)
{
    // Unpack the bitmask into a vector
    Eigen::Matrix<double, N, 1> v;
    for (unsigned i=0; i < N; ++i)
    {
        v(i) = (vert & (1 << i)) ? 1 : -1;
    }

    Eigen::Matrix<double, N, 1> v_ = rot * v;
    uint8_t vert_ = 0;
    for (unsigned i=0; i < N; ++i)
    {
        vert_ |= (v_(i) > 0) << i;
    }
    return vert_;
}

template <unsigned N>
std::unique_ptr<MarchingTable<N>> buildTable()
{
    std::unique_ptr<MarchingTable<N>> _table(new MarchingTable<N>);
    auto& table = *_table;

    // Mark every case as uninitialized
    for (auto& t : table)
    {
        t[0][0].first = -1;
    }

    // Load the initial set of cases (specialized on a per-dimension basis)
    loadCases<N>(table);

    //  Load all possible rigid-body rotations, which we will use to populate
    //  the rest of the table
    auto rots = rigidRotations<N>();

    //  Start by marking the changed elements on the table
    std::array<bool, _pow(2, N)> changed;
    for (unsigned i=0; i < table.size(); ++i)
    {
        changed[i] = table[i][0][0].first != -1;
    }

    // Loop until the system stabilizes
    bool any_changed = true;
    while (any_changed)
    {
        any_changed = false;
        for (unsigned i=0; i < table.size(); ++i)
        {
            // If this vertex bitmask has changed in the previous cycle,
            // then apply every possible rotation to fill out the table.
            if (changed[i])
            {
                changed[i] = false;
                for (const auto& rot : rots)
                {
                    const Patches<N>& patches = table[i];
                    auto i_ = applyRotation<N>(i, rot);
                    Patches<N>& target = table[i_];

                    // If this new target is uninitialized, then populate it
                    // by applying the rigid rotation to all the patch edges
                    if (target[0][0].first == -1)
                    {
                        changed[i_] = true;
                        any_changed = true;

                        // Iterate over patches
                        for (unsigned p=0; p < patches.size() &&
                                           patches[p][0].first != -1; ++p)
                        {
                            // Iterate over patch edges
                            for (unsigned e=0; e < patches[p].size() &&
                                               patches[p][e].first != -1; ++e)
                            {
                                target[p][e] = {
                                    applyRotation<N>(patches[p][e].first, rot),
                                    applyRotation<N>(patches[p][e].second, rot)
                                };
                            }
                        }
                    }
                }
            }
        }
    }

    return _table;
}

template std::unique_ptr<MarchingTable<2>> buildTable<2>();
template std::unique_ptr<MarchingTable<3>> buildTable<3>();
