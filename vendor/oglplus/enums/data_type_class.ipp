//  File implement/oglplus/enums/data_type_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/data_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<DataType> class Transform>
class EnumToClass<Base, DataType, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_BYTE
# if defined Byte
#  pragma push_macro("Byte")
#  undef Byte
	Transform<DataType::Byte> Byte;
#  pragma pop_macro("Byte")
# else
	Transform<DataType::Byte> Byte;
# endif
#endif
#if defined GL_SHORT
# if defined Short
#  pragma push_macro("Short")
#  undef Short
	Transform<DataType::Short> Short;
#  pragma pop_macro("Short")
# else
	Transform<DataType::Short> Short;
# endif
#endif
#if defined GL_INT
# if defined Int
#  pragma push_macro("Int")
#  undef Int
	Transform<DataType::Int> Int;
#  pragma pop_macro("Int")
# else
	Transform<DataType::Int> Int;
# endif
#endif
#if defined GL_FIXED
# if defined Fixed
#  pragma push_macro("Fixed")
#  undef Fixed
	Transform<DataType::Fixed> Fixed;
#  pragma pop_macro("Fixed")
# else
	Transform<DataType::Fixed> Fixed;
# endif
#endif
#if defined GL_FLOAT
# if defined Float
#  pragma push_macro("Float")
#  undef Float
	Transform<DataType::Float> Float;
#  pragma pop_macro("Float")
# else
	Transform<DataType::Float> Float;
# endif
#endif
#if defined GL_HALF_FLOAT
# if defined HalfFloat
#  pragma push_macro("HalfFloat")
#  undef HalfFloat
	Transform<DataType::HalfFloat> HalfFloat;
#  pragma pop_macro("HalfFloat")
# else
	Transform<DataType::HalfFloat> HalfFloat;
# endif
#endif
#if defined GL_DOUBLE
# if defined Double
#  pragma push_macro("Double")
#  undef Double
	Transform<DataType::Double> Double;
#  pragma pop_macro("Double")
# else
	Transform<DataType::Double> Double;
# endif
#endif
#if defined GL_UNSIGNED_BYTE
# if defined UnsignedByte
#  pragma push_macro("UnsignedByte")
#  undef UnsignedByte
	Transform<DataType::UnsignedByte> UnsignedByte;
#  pragma pop_macro("UnsignedByte")
# else
	Transform<DataType::UnsignedByte> UnsignedByte;
# endif
#endif
#if defined GL_UNSIGNED_SHORT
# if defined UnsignedShort
#  pragma push_macro("UnsignedShort")
#  undef UnsignedShort
	Transform<DataType::UnsignedShort> UnsignedShort;
#  pragma pop_macro("UnsignedShort")
# else
	Transform<DataType::UnsignedShort> UnsignedShort;
# endif
#endif
#if defined GL_UNSIGNED_INT
# if defined UnsignedInt
#  pragma push_macro("UnsignedInt")
#  undef UnsignedInt
	Transform<DataType::UnsignedInt> UnsignedInt;
#  pragma pop_macro("UnsignedInt")
# else
	Transform<DataType::UnsignedInt> UnsignedInt;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_BYTE
# if defined Byte
#  pragma push_macro("Byte")
#  undef Byte
	 , Byte(_base())
#  pragma pop_macro("Byte")
# else
	 , Byte(_base())
# endif
#endif
#if defined GL_SHORT
# if defined Short
#  pragma push_macro("Short")
#  undef Short
	 , Short(_base())
#  pragma pop_macro("Short")
# else
	 , Short(_base())
# endif
#endif
#if defined GL_INT
# if defined Int
#  pragma push_macro("Int")
#  undef Int
	 , Int(_base())
#  pragma pop_macro("Int")
# else
	 , Int(_base())
# endif
#endif
#if defined GL_FIXED
# if defined Fixed
#  pragma push_macro("Fixed")
#  undef Fixed
	 , Fixed(_base())
#  pragma pop_macro("Fixed")
# else
	 , Fixed(_base())
# endif
#endif
#if defined GL_FLOAT
# if defined Float
#  pragma push_macro("Float")
#  undef Float
	 , Float(_base())
#  pragma pop_macro("Float")
# else
	 , Float(_base())
# endif
#endif
#if defined GL_HALF_FLOAT
# if defined HalfFloat
#  pragma push_macro("HalfFloat")
#  undef HalfFloat
	 , HalfFloat(_base())
#  pragma pop_macro("HalfFloat")
# else
	 , HalfFloat(_base())
# endif
#endif
#if defined GL_DOUBLE
# if defined Double
#  pragma push_macro("Double")
#  undef Double
	 , Double(_base())
#  pragma pop_macro("Double")
# else
	 , Double(_base())
# endif
#endif
#if defined GL_UNSIGNED_BYTE
# if defined UnsignedByte
#  pragma push_macro("UnsignedByte")
#  undef UnsignedByte
	 , UnsignedByte(_base())
#  pragma pop_macro("UnsignedByte")
# else
	 , UnsignedByte(_base())
# endif
#endif
#if defined GL_UNSIGNED_SHORT
# if defined UnsignedShort
#  pragma push_macro("UnsignedShort")
#  undef UnsignedShort
	 , UnsignedShort(_base())
#  pragma pop_macro("UnsignedShort")
# else
	 , UnsignedShort(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT
# if defined UnsignedInt
#  pragma push_macro("UnsignedInt")
#  undef UnsignedInt
	 , UnsignedInt(_base())
#  pragma pop_macro("UnsignedInt")
# else
	 , UnsignedInt(_base())
# endif
#endif
	{ }
};

} // namespace enums

