//  File implement/oglplus/enums/buffer_map_access_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/buffer_map_access.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<BufferMapAccess> class Transform>
class EnumToClass<Base, BufferMapAccess, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_MAP_READ_BIT
# if defined Read
#  pragma push_macro("Read")
#  undef Read
	Transform<BufferMapAccess::Read> Read;
#  pragma pop_macro("Read")
# else
	Transform<BufferMapAccess::Read> Read;
# endif
#endif
#if defined GL_MAP_WRITE_BIT
# if defined Write
#  pragma push_macro("Write")
#  undef Write
	Transform<BufferMapAccess::Write> Write;
#  pragma pop_macro("Write")
# else
	Transform<BufferMapAccess::Write> Write;
# endif
#endif
#if defined GL_MAP_PERSISTENT_BIT
# if defined Persistent
#  pragma push_macro("Persistent")
#  undef Persistent
	Transform<BufferMapAccess::Persistent> Persistent;
#  pragma pop_macro("Persistent")
# else
	Transform<BufferMapAccess::Persistent> Persistent;
# endif
#endif
#if defined GL_MAP_COHERENT_BIT
# if defined Coherent
#  pragma push_macro("Coherent")
#  undef Coherent
	Transform<BufferMapAccess::Coherent> Coherent;
#  pragma pop_macro("Coherent")
# else
	Transform<BufferMapAccess::Coherent> Coherent;
# endif
#endif
#if defined GL_MAP_INVALIDATE_RANGE_BIT
# if defined InvalidateRange
#  pragma push_macro("InvalidateRange")
#  undef InvalidateRange
	Transform<BufferMapAccess::InvalidateRange> InvalidateRange;
#  pragma pop_macro("InvalidateRange")
# else
	Transform<BufferMapAccess::InvalidateRange> InvalidateRange;
# endif
#endif
#if defined GL_MAP_INVALIDATE_BUFFER_BIT
# if defined InvalidateBuffer
#  pragma push_macro("InvalidateBuffer")
#  undef InvalidateBuffer
	Transform<BufferMapAccess::InvalidateBuffer> InvalidateBuffer;
#  pragma pop_macro("InvalidateBuffer")
# else
	Transform<BufferMapAccess::InvalidateBuffer> InvalidateBuffer;
# endif
#endif
#if defined GL_MAP_FLUSH_EXPLICIT_BIT
# if defined FlushExplicit
#  pragma push_macro("FlushExplicit")
#  undef FlushExplicit
	Transform<BufferMapAccess::FlushExplicit> FlushExplicit;
#  pragma pop_macro("FlushExplicit")
# else
	Transform<BufferMapAccess::FlushExplicit> FlushExplicit;
# endif
#endif
#if defined GL_MAP_UNSYNCHRONIZED_BIT
# if defined Unsynchronized
#  pragma push_macro("Unsynchronized")
#  undef Unsynchronized
	Transform<BufferMapAccess::Unsynchronized> Unsynchronized;
#  pragma pop_macro("Unsynchronized")
# else
	Transform<BufferMapAccess::Unsynchronized> Unsynchronized;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_MAP_READ_BIT
# if defined Read
#  pragma push_macro("Read")
#  undef Read
	 , Read(_base())
#  pragma pop_macro("Read")
# else
	 , Read(_base())
# endif
#endif
#if defined GL_MAP_WRITE_BIT
# if defined Write
#  pragma push_macro("Write")
#  undef Write
	 , Write(_base())
#  pragma pop_macro("Write")
# else
	 , Write(_base())
# endif
#endif
#if defined GL_MAP_PERSISTENT_BIT
# if defined Persistent
#  pragma push_macro("Persistent")
#  undef Persistent
	 , Persistent(_base())
#  pragma pop_macro("Persistent")
# else
	 , Persistent(_base())
# endif
#endif
#if defined GL_MAP_COHERENT_BIT
# if defined Coherent
#  pragma push_macro("Coherent")
#  undef Coherent
	 , Coherent(_base())
#  pragma pop_macro("Coherent")
# else
	 , Coherent(_base())
# endif
#endif
#if defined GL_MAP_INVALIDATE_RANGE_BIT
# if defined InvalidateRange
#  pragma push_macro("InvalidateRange")
#  undef InvalidateRange
	 , InvalidateRange(_base())
#  pragma pop_macro("InvalidateRange")
# else
	 , InvalidateRange(_base())
# endif
#endif
#if defined GL_MAP_INVALIDATE_BUFFER_BIT
# if defined InvalidateBuffer
#  pragma push_macro("InvalidateBuffer")
#  undef InvalidateBuffer
	 , InvalidateBuffer(_base())
#  pragma pop_macro("InvalidateBuffer")
# else
	 , InvalidateBuffer(_base())
# endif
#endif
#if defined GL_MAP_FLUSH_EXPLICIT_BIT
# if defined FlushExplicit
#  pragma push_macro("FlushExplicit")
#  undef FlushExplicit
	 , FlushExplicit(_base())
#  pragma pop_macro("FlushExplicit")
# else
	 , FlushExplicit(_base())
# endif
#endif
#if defined GL_MAP_UNSYNCHRONIZED_BIT
# if defined Unsynchronized
#  pragma push_macro("Unsynchronized")
#  undef Unsynchronized
	 , Unsynchronized(_base())
#  pragma pop_macro("Unsynchronized")
# else
	 , Unsynchronized(_base())
# endif
#endif
	{ }
};

} // namespace enums

