/**
 *  @file oglplus/text/bitmap_glyph/pager.ipp
 *  @brief Implementation of Bitmap-font-based text rendering, page swapping
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */
#include <cassert>

namespace oglplus {
namespace text {

OGLPLUS_LIB_FUNC
bool BitmapGlyphPager::_frames_consistent(void) const
{
	_gpu_page_map.Bind(Buffer::Target::Uniform);
	BufferTypedMap<gpu_frame_t> page_frame_map(
		BufferTarget::Uniform,
		BufferMapAccess::Read
	);
	const std::size_t n = _frames.size();
	for(std::size_t i=0; i!=n; ++i)
	{
		GLint page = _frames[i];
		if(page >= 0)
		{
			for(std::size_t j=0; j!=i; ++j)
			{
				if(page == _frames[j])
				{
					return false;
				}
			}

			assert(!(page < 0));

			gpu_frame_t frame = page_frame_map.At(GLuint(page));
			if(frame != gpu_frame_t(i))
			{
				return false;
			}
		}
	}
	return true;
}

OGLPLUS_LIB_FUNC
bool BitmapGlyphPager::_page_in_frames(GLuint page) const
{
	for(auto i=_frames.begin(), e=_frames.end(); i!=e; ++i)
	{
		if(*i == GLint(page))
		{
			return true;
		}
	}
	return false;
}

OGLPLUS_LIB_FUNC
void BitmapGlyphPager::_replace_page(GLuint frame, GLuint page)
{
	assert(!_page_in_frames(page));
	// the previous page in the frame
	GLint previous = _frames[frame];
	// assign the new page to the frame
	_frames[frame] = GLint(page);
	// update the age of the other frames
	Update();
	// initialize the age of the frame
	_ages[frame] = _init_age();
	//
	// we'll need to update the page map on gpu
	_gpu_page_map.Bind(Buffer::Target::Uniform);
	BufferTypedMap<gpu_frame_t> page_frame_map(
		BufferTarget::Uniform,
		BufferMapAccess::Write
	);
	if(previous >= 0)
	{
		// remove the previous page from active list
		_active_pages.erase(GLuint(previous));
		page_frame_map.At(GLuint(previous)) = _invalid_gpu_frame();
		// add the new page into the active list
	}
	_active_pages[page] = frame;
	page_frame_map.At(page) = gpu_frame_t(frame);
}

OGLPLUS_LIB_FUNC
BitmapGlyphPager::BitmapGlyphPager(
	BitmapGlyphRenderingBase& parent,
	TextureUnitSelector pg_map_tex_unit,
	SizeType frame_count
): _parent(parent)
 , _frames(frame_count, GLint(-1))
 , _ages(frame_count, _zero_age())
 , _pg_map_tex_unit(pg_map_tex_unit)
{
	_gpu_page_map.Bind(Buffer::Target::Uniform);
	std::vector<gpu_frame_t> data(
		BitmapGlyphPlaneCount(_parent)*
		BitmapGlyphPagesPerPlane(_parent),
		_invalid_gpu_frame()
	);
	Buffer::Data(Buffer::Target::Uniform, data);

	Texture::Active(_pg_map_tex_unit);
	_page_map_tex.Bind(Texture::Target::Buffer);
	Texture::Buffer(
		Texture::Target::Buffer,
		PixelDataInternalFormat::R8UI,
		_gpu_page_map
	);

	assert(_is_ok());
}

OGLPLUS_LIB_FUNC
GLuint BitmapGlyphPager::FindFrame(void)
{
	assert(_is_ok());

	const std::size_t n = _frames.size();

	for(std::size_t i=0; i!=n; ++i)
	{
		if(_frames[i] < 0)
		{
			return GLuint(i);
		}
	}

	age_t min = _full_age();
	std::size_t m = 0;
	for(std::size_t i=0; i!=n; ++i)
	{
		if(min > _ages[i])
		{
			min = _ages[i];
			m = i;
		}
	}
	return GLuint(m);
}

OGLPLUS_LIB_FUNC
bool BitmapGlyphPager::UsePage(GLuint page)
{
	assert(_is_ok());

	auto pos = _active_pages.find(page);
	// if this is a page miss
	if(pos == _active_pages.end())
	{
		assert(!_page_in_frames(page));
		return false;
	}
	// if this is a page hit
	else
	{
		assert(_page_in_frames(page));
		// note frame usage
		_touch_frame(pos->second);
	}
	return true;
}

OGLPLUS_LIB_FUNC
GLint BitmapGlyphPager::FrameOfPage(GLuint page) const
{
	auto pos = _active_pages.find(page);
	if(pos != _active_pages.end())
	{
		return GLint(pos->second);
	}
	else return GLint(-1);
}

} // namespace text
} // namespace oglplus

