/**
 *  @file oglplus/text/bitmap_glyph/layout_storage.ipp
 *  @brief Implementation of Bitmap-font-based text rendering layout storage
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {
namespace text {

OGLPLUS_LIB_FUNC
BitmapGlyphLayoutStorage::BitmapGlyphLayoutStorage(
	BitmapGlyphRenderingBase& parent,
	SizeType capacity,
	SizeType alloc_unit
): _parent(parent)
 , _list_head(0u)
 , _free(capacity)
 , _capacity(capacity)
 , _alloc_unit(alloc_unit)
{
	assert(_alloc_unit % 2 == 0);

	_vao.Bind();
	{
		_code_points.Bind(Buffer::Target::Array);
		Buffer::Data<GLuint>(
			Buffer::Target::Array,
			_capacity,
			nullptr
		);

		GLuint _linked_list[2] = {GLuint(_capacity), _list_nil()};
		Buffer::SubData(
			Buffer::Target::Array,
			BufferTypedSize<GLuint>(_list_head),
			sizeof(_linked_list)/sizeof(_linked_list[0]),
			_linked_list
		);

		VertexAttribSlot location(0);
		VertexArrayAttrib attr(location);
		attr.Setup<GLuint>();
		attr.Enable();
	}
	{
		_x_offsets.Bind(Buffer::Target::Array);
		Buffer::Data<GLfloat>(
			Buffer::Target::Array,
			_capacity,
			nullptr
		);

		VertexAttribSlot location(1);
		VertexArrayAttrib attr(location);
		attr.Setup<GLfloat>();
		attr.Enable();
	}

	NoVertexArray().Bind();
}

OGLPLUS_LIB_FUNC
bool BitmapGlyphLayoutStorage::Allocate(BitmapGlyphLayoutData& layout_data)
{
	GLsizei required = layout_data._capacity;
	assert(required > 0);
	// adjust the allocation size to be a multiple
	// of the allocation unit
	GLint pad = required % _alloc_unit;
	if(pad != 0)
	{
		required += _alloc_unit - pad;
	}
	// do a quick initial check if the allocation
	// can be satisfied
	if(required > _free) return false;
	//
	// now traverse the linked list (stored in the free
	// space in _code_points) to find the best chunk
	_code_points.Bind(Buffer::Target::Array);
	BufferTypedMap<GLuint> map(
		BufferTarget::Array,
		BufferMapAccess::Read|BufferMapAccess::Write
	);
	//
	// the position in the linked list
	GLuint list_pos = _list_head;
	// the best position in the linked list
	GLuint best_pos = _list_nil();
	// previous position before the best one
	GLuint prev_pos = _list_nil();
	// a temporary list pointer
	GLuint temp_pos = _list_nil();
	// best difference between the request and available space
	GLsizei min_diff = _capacity;
	while(list_pos != _list_nil())
	{
		// available space in the current chunk
		GLsizei available = GLsizei(map.At(list_pos));
		// the difference between the request
		// and the current chunk
		GLsizei diff = available - required;
		// if we have an ideal fit
		if(diff == 0)
		{
			prev_pos = temp_pos;
			best_pos = list_pos;
			min_diff = 0;
			break;
		}
		// if it fits but not ideally
		else if(diff > 0)
		{
			// if it is a better fit than the previous
			if(min_diff > diff)
			{
				// remember it
				prev_pos = temp_pos;
				best_pos = list_pos;
				min_diff = diff;
			}
		}
		// remember the previous list element
		temp_pos = list_pos;
		// try the next chunk
		list_pos = map.At(list_pos+1);
	}
	assert(min_diff >= 0);
	//
	// if we haven't found a suitable chunk
	// report failure
	if(best_pos == _list_nil()) return false;

	// otherwise ...
	// the next element
	GLuint next_pos;
	// if there is some free space in the chunk left
	if(min_diff > 0)
	{
		// insert a new list element
		next_pos = best_pos + GLuint(required);
		map.At(next_pos) = GLuint(min_diff);
		map.At(next_pos+1) = map.At(best_pos+1);
	}
	// if it fits perfectly
	else
	{
		// find the next element
		next_pos = map.At(best_pos+1);
	}
	//
	// if this was the first element in the list
	if(prev_pos == _list_nil())
	{
		// update the head
		_list_head = next_pos;
	}
	else
	{
		// update the previous element
		map.At(prev_pos+1) = next_pos;
	}
	_free -= required;
	assert(_list_head != _list_nil() || _free == 0);

	// update the layout data
	layout_data._offset = GLint(best_pos);
	layout_data._length = GLsizei(0);
	layout_data._capacity = GLsizei(required);
	layout_data._storage = this;
	layout_data._width = 0.0f;
	// success
	return true;
}

OGLPLUS_LIB_FUNC
void BitmapGlyphLayoutStorage::Deallocate(BitmapGlyphLayoutData& layout_data)
{
	assert(layout_data._offset >= 0);
	assert(layout_data._offset < _capacity);
	assert(layout_data._storage == this);

	const GLuint old_pos = GLuint(layout_data._offset);
	const GLuint returned = GLuint(layout_data._capacity);
	//
	_code_points.Bind(Buffer::Target::Array);
	BufferTypedMap<GLuint> map(
		BufferTarget::Array,
		BufferMapAccess::Read|BufferMapAccess::Write
	);
	// if the storage is completelly full
	if(_list_head == _list_nil())
	{
		assert(_free == 0);
		map.At(old_pos) = returned;
		map.At(old_pos+1) = _list_nil();
		_list_head = old_pos;
	}
	else
	{
		GLuint next_pos = _list_head;
		GLuint prev_pos = _list_nil();
		// go through the list until you find
		// the next free chunk after the one
		// that is currently being freed
		while(next_pos < old_pos && next_pos != _list_nil())
		{
			prev_pos = next_pos;
			next_pos = map.At(next_pos+1);
		}
		// whether it is adjacent to the next element
		bool adj_to_next =
			(old_pos + returned == next_pos) &&
			(next_pos != _list_nil());
		// if there is no previous element
		if(prev_pos == _list_nil())
		{
			// if the freed is adjacent to the next
			if(adj_to_next)
			{
				// merge them
				GLuint next_size = map.At(next_pos);
				map.At(old_pos) = returned + next_size;
				map.At(old_pos+1) = map.At(next_pos+1);
			}
			else
			{
				// otherwise insert new element
				map.At(old_pos) = returned;
				map.At(old_pos+1) = next_pos;
			}
			_list_head = old_pos;
		}
		else
		{
			GLuint prev_size = map.At(prev_pos);
			// whether it is adjacent to the previous element
			bool adj_to_prev = prev_pos+prev_size == old_pos;
			//
			// if the freed is adjacent to both
			// the previous and the next free chunk
			if(adj_to_prev && adj_to_next)
			{
				GLuint next_size = map.At(next_pos);
				map.At(prev_pos) =
					prev_size +
					returned +
					next_size;
				map.At(prev_pos+1) = map.At(next_pos+1);
			}
			else if(adj_to_prev)
			{
				map.At(prev_pos) = prev_size + returned;
			}
			else if(adj_to_next)
			{
				GLuint next_size = map.At(next_pos);
				map.At(old_pos) = returned + next_size;
				map.At(old_pos+1) = map.At(next_pos+1);
			}
			else
			{
				map.At(prev_pos+1) = old_pos;
				map.At(old_pos) = returned;
				map.At(old_pos+1) = next_pos;
			}
		}
	}

	_free += returned;

	layout_data._offset = -1;
	layout_data._length = 0;
	layout_data._capacity = 0;
	layout_data._storage = nullptr;
	layout_data._width = 0.0f;
}

OGLPLUS_LIB_FUNC
void BitmapGlyphLayoutStorage::Initialize(
	BitmapGlyphLayoutData& layout_data,
	GLfloat width,
	const std::vector<GLfloat>& x_offsets,
	const CodePoint* cps,
	SizeType length
)
{
	assert(layout_data._capacity >= length);

	// set the length
	layout_data._length = length;
	// upload the code points
	std::vector<GLuint> code_points(cps, cps+GLsizei(length));
	_code_points.Bind(Buffer::Target::Array);
	Buffer::SubData(
		Buffer::Target::Array,
		BufferTypedSize<GLuint>(layout_data._offset),
		GLsizei(code_points.size()),
		code_points.data()
	);
	// upload the x-offsets
	layout_data._width = width;
	_x_offsets.Bind(Buffer::Target::Array);
	Buffer::SubData(
		Buffer::Target::Array,
		BufferTypedSize<GLuint>(layout_data._offset),
		GLsizei(x_offsets.size()),
		x_offsets.data()
	);
}

} // namespace text
} // namespace oglplus

