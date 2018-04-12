#include "libfive/tree/tree.hpp"
#include "libfive/render/brep/mesh.hpp"

int main(int argc, char** argv)
{
    (void)argc;
    (void)argv;

    for (int i = 0; ; i++)
    {
        printf("%li\n", sizeof(Kernel::XTree<3>));
        auto t = sqrt(square(Kernel::Tree::X()) +
                      square(Kernel::Tree::Y()) +
                      square(Kernel::Tree::Z())) - 10;
        Kernel::Region<3> region({ -11, -11, -11 }, { 11, 11, 11 });
        float minFeat = .2, maxErr = .000001;
        time_t t0 = time(nullptr);
        auto mesh = Kernel::XTree<3>::build(t, region, minFeat, maxErr, false);
        time_t t1 = time(nullptr);
        time_t tdiff = t1 - t0;
        printf("sphere: %lu seconds\n", tdiff);
    }
}
