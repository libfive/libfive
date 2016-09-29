# Meshing
> T. Ju, F. Losasso, S. Schaefer, and J. Warren,
> "Dual contouring of hermite data,"
> ACM Transactions on Graphics, vol. 21, no. 3, pp. 339– 346,
> July 2002, iSSN 0730-0301 (Proceedings of ACM SIGGRAPH 2002).

Everyone's favorite meshing technique: it's hierarchical, water-tight,
preserves sharp corners, and is somewhat reasonable to implement.
([link](http://www.frankpetterson.com/publications/dualcontour/dualcontour.pdf))

> S. Schaefer, and J. Warren
> "Dual Contouring: The Secret Sauce"
> 2003

This write-up does a good job of unpacking the implementation details that
were left out of the conference paper.  Major props to the authors for
publishing it; this is a probably significant reason for why dual contouring
has been so widely adopted.
([link](https://people.eecs.berkeley.edu/~jrs/meshpapers/SchaeferWarren2.pdf))

> Gerstner, T., and Pajarola, R. 2000.
> "Topology preserving and controlled topology simplifying multiresolution isosurface extraction."
> IEEE Visualization 2000, 259–266.

I haven't internalized everything in this paper, but section 4.3 describes a
clever automatic way to build n-dimensional tables of topological safety, i.e.
whether you can collapse a cube with particular corners set.  This technique
is referenced in the dual contouring papers above.
([link](http://wissrech.ins.uni-bonn.de/research/pub/gerstner/topo.pdf))

> Kobbelt, L. P., Botsch, M., Schwanecke, U., and Seidel, H.-P. 2001.
> "Feature-sensitive surface extraction from volume data,"
> Proceedings of SIGGRAPH 2001, ACM Press / ACM SIGGRAPH,
> Computer Graphics Proceedings, Annual Conference Series, 57–66.

This paper describes a technique to position vertices on sharp features,
similar to the QEF minimization used in dual contouring.
([link](https://www.graphics.rwth-aachen.de/media/papers/feature1.pdf))
