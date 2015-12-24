/**
 *  @file oglplus/images/voronoi.ipp
 *  @brief Implementation of images::VoronoiDiagram and images::VoronoiCells
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/images/cell.hpp>
#include <oglplus/lib/incl_end.ipp>

namespace oglplus {
namespace images {

struct VoronoiNearestPointColor
{
	Vec3d operator()(
		const GLdouble* dists,
		const Vec3d* colors,
		GLsizei count
	) const
	{
		double md = 2.0;
		GLsizei mc = 0;

		for(GLsizei c=0; c<count; ++c)
		{
			if(md > dists[c])
			{
				md = dists[c];
				mc = c;
			}
		}
		return colors[mc];
	}
};

OGLPLUS_LIB_FUNC
VoronoiDiagram::VoronoiDiagram(
	SizeType cell_w,
	SizeType cell_h,
	SizeType cell_d,
	const Image& input
): Image(static_cast<Image&&>(
	CellImageGen<GLubyte, 3>(
		cell_w, cell_h, cell_d,
		input,
		CellImageGen<GLubyte, 3>::EulerDistance(),
		VoronoiNearestPointColor()
	)
))
{ }

struct VoronoiCellDistance
{
	GLdouble operator()(const std::vector<GLdouble>& d) const
	{
		assert(!d.empty());
		return d.front();
	}
};

OGLPLUS_LIB_FUNC
VoronoiCells::VoronoiCells(
	SizeType cell_w,
	SizeType cell_h,
	SizeType cell_d,
	const Image& input
): Image(static_cast<Image&&>(
	WorleyCellGen(
		cell_w, cell_h, cell_d,
		input,
		VoronoiCellDistance(), 1
	)
))
{ }

} // namespace images
} // namespace oglplus

