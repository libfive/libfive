//  File implement/oglplus/enums/ext/nv_path_fill_cover_mode_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_fill_cover_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PathNVFillCoverMode> class Transform>
class EnumToClass<Base, PathNVFillCoverMode, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_CONVEX_HULL_NV
# if defined ConvexHull
#  pragma push_macro("ConvexHull")
#  undef ConvexHull
	Transform<PathNVFillCoverMode::ConvexHull> ConvexHull;
#  pragma pop_macro("ConvexHull")
# else
	Transform<PathNVFillCoverMode::ConvexHull> ConvexHull;
# endif
#endif
#if defined GL_BOUNDING_BOX_NV
# if defined BoundingBox
#  pragma push_macro("BoundingBox")
#  undef BoundingBox
	Transform<PathNVFillCoverMode::BoundingBox> BoundingBox;
#  pragma pop_macro("BoundingBox")
# else
	Transform<PathNVFillCoverMode::BoundingBox> BoundingBox;
# endif
#endif
#if defined GL_BOUNDING_BOX_OF_BOUNDING_BOXES_NV
# if defined BoundingBoxOfBoundingBoxes
#  pragma push_macro("BoundingBoxOfBoundingBoxes")
#  undef BoundingBoxOfBoundingBoxes
	Transform<PathNVFillCoverMode::BoundingBoxOfBoundingBoxes> BoundingBoxOfBoundingBoxes;
#  pragma pop_macro("BoundingBoxOfBoundingBoxes")
# else
	Transform<PathNVFillCoverMode::BoundingBoxOfBoundingBoxes> BoundingBoxOfBoundingBoxes;
# endif
#endif
#if defined GL_PATH_FILL_COVER_MODE_NV
# if defined FillCover
#  pragma push_macro("FillCover")
#  undef FillCover
	Transform<PathNVFillCoverMode::FillCover> FillCover;
#  pragma pop_macro("FillCover")
# else
	Transform<PathNVFillCoverMode::FillCover> FillCover;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_CONVEX_HULL_NV
# if defined ConvexHull
#  pragma push_macro("ConvexHull")
#  undef ConvexHull
	 , ConvexHull(_base())
#  pragma pop_macro("ConvexHull")
# else
	 , ConvexHull(_base())
# endif
#endif
#if defined GL_BOUNDING_BOX_NV
# if defined BoundingBox
#  pragma push_macro("BoundingBox")
#  undef BoundingBox
	 , BoundingBox(_base())
#  pragma pop_macro("BoundingBox")
# else
	 , BoundingBox(_base())
# endif
#endif
#if defined GL_BOUNDING_BOX_OF_BOUNDING_BOXES_NV
# if defined BoundingBoxOfBoundingBoxes
#  pragma push_macro("BoundingBoxOfBoundingBoxes")
#  undef BoundingBoxOfBoundingBoxes
	 , BoundingBoxOfBoundingBoxes(_base())
#  pragma pop_macro("BoundingBoxOfBoundingBoxes")
# else
	 , BoundingBoxOfBoundingBoxes(_base())
# endif
#endif
#if defined GL_PATH_FILL_COVER_MODE_NV
# if defined FillCover
#  pragma push_macro("FillCover")
#  undef FillCover
	 , FillCover(_base())
#  pragma pop_macro("FillCover")
# else
	 , FillCover(_base())
# endif
#endif
	{ }
};

} // namespace enums

