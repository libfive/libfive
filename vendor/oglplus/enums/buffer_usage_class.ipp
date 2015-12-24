//  File implement/oglplus/enums/buffer_usage_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/buffer_usage.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<BufferUsage> class Transform>
class EnumToClass<Base, BufferUsage, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_STREAM_DRAW
# if defined StreamDraw
#  pragma push_macro("StreamDraw")
#  undef StreamDraw
	Transform<BufferUsage::StreamDraw> StreamDraw;
#  pragma pop_macro("StreamDraw")
# else
	Transform<BufferUsage::StreamDraw> StreamDraw;
# endif
#endif
#if defined GL_STREAM_READ
# if defined StreamRead
#  pragma push_macro("StreamRead")
#  undef StreamRead
	Transform<BufferUsage::StreamRead> StreamRead;
#  pragma pop_macro("StreamRead")
# else
	Transform<BufferUsage::StreamRead> StreamRead;
# endif
#endif
#if defined GL_STREAM_COPY
# if defined StreamCopy
#  pragma push_macro("StreamCopy")
#  undef StreamCopy
	Transform<BufferUsage::StreamCopy> StreamCopy;
#  pragma pop_macro("StreamCopy")
# else
	Transform<BufferUsage::StreamCopy> StreamCopy;
# endif
#endif
#if defined GL_STATIC_DRAW
# if defined StaticDraw
#  pragma push_macro("StaticDraw")
#  undef StaticDraw
	Transform<BufferUsage::StaticDraw> StaticDraw;
#  pragma pop_macro("StaticDraw")
# else
	Transform<BufferUsage::StaticDraw> StaticDraw;
# endif
#endif
#if defined GL_STATIC_READ
# if defined StaticRead
#  pragma push_macro("StaticRead")
#  undef StaticRead
	Transform<BufferUsage::StaticRead> StaticRead;
#  pragma pop_macro("StaticRead")
# else
	Transform<BufferUsage::StaticRead> StaticRead;
# endif
#endif
#if defined GL_STATIC_COPY
# if defined StaticCopy
#  pragma push_macro("StaticCopy")
#  undef StaticCopy
	Transform<BufferUsage::StaticCopy> StaticCopy;
#  pragma pop_macro("StaticCopy")
# else
	Transform<BufferUsage::StaticCopy> StaticCopy;
# endif
#endif
#if defined GL_DYNAMIC_DRAW
# if defined DynamicDraw
#  pragma push_macro("DynamicDraw")
#  undef DynamicDraw
	Transform<BufferUsage::DynamicDraw> DynamicDraw;
#  pragma pop_macro("DynamicDraw")
# else
	Transform<BufferUsage::DynamicDraw> DynamicDraw;
# endif
#endif
#if defined GL_DYNAMIC_READ
# if defined DynamicRead
#  pragma push_macro("DynamicRead")
#  undef DynamicRead
	Transform<BufferUsage::DynamicRead> DynamicRead;
#  pragma pop_macro("DynamicRead")
# else
	Transform<BufferUsage::DynamicRead> DynamicRead;
# endif
#endif
#if defined GL_DYNAMIC_COPY
# if defined DynamicCopy
#  pragma push_macro("DynamicCopy")
#  undef DynamicCopy
	Transform<BufferUsage::DynamicCopy> DynamicCopy;
#  pragma pop_macro("DynamicCopy")
# else
	Transform<BufferUsage::DynamicCopy> DynamicCopy;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_STREAM_DRAW
# if defined StreamDraw
#  pragma push_macro("StreamDraw")
#  undef StreamDraw
	 , StreamDraw(_base())
#  pragma pop_macro("StreamDraw")
# else
	 , StreamDraw(_base())
# endif
#endif
#if defined GL_STREAM_READ
# if defined StreamRead
#  pragma push_macro("StreamRead")
#  undef StreamRead
	 , StreamRead(_base())
#  pragma pop_macro("StreamRead")
# else
	 , StreamRead(_base())
# endif
#endif
#if defined GL_STREAM_COPY
# if defined StreamCopy
#  pragma push_macro("StreamCopy")
#  undef StreamCopy
	 , StreamCopy(_base())
#  pragma pop_macro("StreamCopy")
# else
	 , StreamCopy(_base())
# endif
#endif
#if defined GL_STATIC_DRAW
# if defined StaticDraw
#  pragma push_macro("StaticDraw")
#  undef StaticDraw
	 , StaticDraw(_base())
#  pragma pop_macro("StaticDraw")
# else
	 , StaticDraw(_base())
# endif
#endif
#if defined GL_STATIC_READ
# if defined StaticRead
#  pragma push_macro("StaticRead")
#  undef StaticRead
	 , StaticRead(_base())
#  pragma pop_macro("StaticRead")
# else
	 , StaticRead(_base())
# endif
#endif
#if defined GL_STATIC_COPY
# if defined StaticCopy
#  pragma push_macro("StaticCopy")
#  undef StaticCopy
	 , StaticCopy(_base())
#  pragma pop_macro("StaticCopy")
# else
	 , StaticCopy(_base())
# endif
#endif
#if defined GL_DYNAMIC_DRAW
# if defined DynamicDraw
#  pragma push_macro("DynamicDraw")
#  undef DynamicDraw
	 , DynamicDraw(_base())
#  pragma pop_macro("DynamicDraw")
# else
	 , DynamicDraw(_base())
# endif
#endif
#if defined GL_DYNAMIC_READ
# if defined DynamicRead
#  pragma push_macro("DynamicRead")
#  undef DynamicRead
	 , DynamicRead(_base())
#  pragma pop_macro("DynamicRead")
# else
	 , DynamicRead(_base())
# endif
#endif
#if defined GL_DYNAMIC_COPY
# if defined DynamicCopy
#  pragma push_macro("DynamicCopy")
#  undef DynamicCopy
	 , DynamicCopy(_base())
#  pragma pop_macro("DynamicCopy")
# else
	 , DynamicCopy(_base())
# endif
#endif
	{ }
};

} // namespace enums

