//  File implement/oglplus/enums/pixel_data_type_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/pixel_data_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PixelDataType> class Transform>
class EnumToClass<Base, PixelDataType, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_UNSIGNED_BYTE
# if defined UnsignedByte
#  pragma push_macro("UnsignedByte")
#  undef UnsignedByte
	Transform<PixelDataType::UnsignedByte> UnsignedByte;
#  pragma pop_macro("UnsignedByte")
# else
	Transform<PixelDataType::UnsignedByte> UnsignedByte;
# endif
#endif
#if defined GL_BYTE
# if defined Byte
#  pragma push_macro("Byte")
#  undef Byte
	Transform<PixelDataType::Byte> Byte;
#  pragma pop_macro("Byte")
# else
	Transform<PixelDataType::Byte> Byte;
# endif
#endif
#if defined GL_UNSIGNED_SHORT
# if defined UnsignedShort
#  pragma push_macro("UnsignedShort")
#  undef UnsignedShort
	Transform<PixelDataType::UnsignedShort> UnsignedShort;
#  pragma pop_macro("UnsignedShort")
# else
	Transform<PixelDataType::UnsignedShort> UnsignedShort;
# endif
#endif
#if defined GL_SHORT
# if defined Short
#  pragma push_macro("Short")
#  undef Short
	Transform<PixelDataType::Short> Short;
#  pragma pop_macro("Short")
# else
	Transform<PixelDataType::Short> Short;
# endif
#endif
#if defined GL_UNSIGNED_INT
# if defined UnsignedInt
#  pragma push_macro("UnsignedInt")
#  undef UnsignedInt
	Transform<PixelDataType::UnsignedInt> UnsignedInt;
#  pragma pop_macro("UnsignedInt")
# else
	Transform<PixelDataType::UnsignedInt> UnsignedInt;
# endif
#endif
#if defined GL_INT
# if defined Int
#  pragma push_macro("Int")
#  undef Int
	Transform<PixelDataType::Int> Int;
#  pragma pop_macro("Int")
# else
	Transform<PixelDataType::Int> Int;
# endif
#endif
#if defined GL_HALF_FLOAT
# if defined HalfFloat
#  pragma push_macro("HalfFloat")
#  undef HalfFloat
	Transform<PixelDataType::HalfFloat> HalfFloat;
#  pragma pop_macro("HalfFloat")
# else
	Transform<PixelDataType::HalfFloat> HalfFloat;
# endif
#endif
#if defined GL_FLOAT
# if defined Float
#  pragma push_macro("Float")
#  undef Float
	Transform<PixelDataType::Float> Float;
#  pragma pop_macro("Float")
# else
	Transform<PixelDataType::Float> Float;
# endif
#endif
#if defined GL_UNSIGNED_BYTE_3_3_2
# if defined UnsignedByte_3_3_2
#  pragma push_macro("UnsignedByte_3_3_2")
#  undef UnsignedByte_3_3_2
	Transform<PixelDataType::UnsignedByte_3_3_2> UnsignedByte_3_3_2;
#  pragma pop_macro("UnsignedByte_3_3_2")
# else
	Transform<PixelDataType::UnsignedByte_3_3_2> UnsignedByte_3_3_2;
# endif
#endif
#if defined GL_UNSIGNED_BYTE_2_3_3_REV
# if defined UnsignedByte_2_3_3_Rev
#  pragma push_macro("UnsignedByte_2_3_3_Rev")
#  undef UnsignedByte_2_3_3_Rev
	Transform<PixelDataType::UnsignedByte_2_3_3_Rev> UnsignedByte_2_3_3_Rev;
#  pragma pop_macro("UnsignedByte_2_3_3_Rev")
# else
	Transform<PixelDataType::UnsignedByte_2_3_3_Rev> UnsignedByte_2_3_3_Rev;
# endif
#endif
#if defined GL_UNSIGNED_SHORT_5_6_5
# if defined UnsignedShort_5_6_5
#  pragma push_macro("UnsignedShort_5_6_5")
#  undef UnsignedShort_5_6_5
	Transform<PixelDataType::UnsignedShort_5_6_5> UnsignedShort_5_6_5;
#  pragma pop_macro("UnsignedShort_5_6_5")
# else
	Transform<PixelDataType::UnsignedShort_5_6_5> UnsignedShort_5_6_5;
# endif
#endif
#if defined GL_UNSIGNED_SHORT_5_6_5_REV
# if defined UnsignedShort_5_6_5_Rev
#  pragma push_macro("UnsignedShort_5_6_5_Rev")
#  undef UnsignedShort_5_6_5_Rev
	Transform<PixelDataType::UnsignedShort_5_6_5_Rev> UnsignedShort_5_6_5_Rev;
#  pragma pop_macro("UnsignedShort_5_6_5_Rev")
# else
	Transform<PixelDataType::UnsignedShort_5_6_5_Rev> UnsignedShort_5_6_5_Rev;
# endif
#endif
#if defined GL_UNSIGNED_SHORT_4_4_4_4
# if defined UnsignedShort_4_4_4_4
#  pragma push_macro("UnsignedShort_4_4_4_4")
#  undef UnsignedShort_4_4_4_4
	Transform<PixelDataType::UnsignedShort_4_4_4_4> UnsignedShort_4_4_4_4;
#  pragma pop_macro("UnsignedShort_4_4_4_4")
# else
	Transform<PixelDataType::UnsignedShort_4_4_4_4> UnsignedShort_4_4_4_4;
# endif
#endif
#if defined GL_UNSIGNED_SHORT_4_4_4_4_REV
# if defined UnsignedShort_4_4_4_4_Rev
#  pragma push_macro("UnsignedShort_4_4_4_4_Rev")
#  undef UnsignedShort_4_4_4_4_Rev
	Transform<PixelDataType::UnsignedShort_4_4_4_4_Rev> UnsignedShort_4_4_4_4_Rev;
#  pragma pop_macro("UnsignedShort_4_4_4_4_Rev")
# else
	Transform<PixelDataType::UnsignedShort_4_4_4_4_Rev> UnsignedShort_4_4_4_4_Rev;
# endif
#endif
#if defined GL_UNSIGNED_SHORT_5_5_5_1
# if defined UnsignedShort_5_5_5_1
#  pragma push_macro("UnsignedShort_5_5_5_1")
#  undef UnsignedShort_5_5_5_1
	Transform<PixelDataType::UnsignedShort_5_5_5_1> UnsignedShort_5_5_5_1;
#  pragma pop_macro("UnsignedShort_5_5_5_1")
# else
	Transform<PixelDataType::UnsignedShort_5_5_5_1> UnsignedShort_5_5_5_1;
# endif
#endif
#if defined GL_UNSIGNED_SHORT_1_5_5_5_REV
# if defined UnsignedShort_1_5_5_5_Rev
#  pragma push_macro("UnsignedShort_1_5_5_5_Rev")
#  undef UnsignedShort_1_5_5_5_Rev
	Transform<PixelDataType::UnsignedShort_1_5_5_5_Rev> UnsignedShort_1_5_5_5_Rev;
#  pragma pop_macro("UnsignedShort_1_5_5_5_Rev")
# else
	Transform<PixelDataType::UnsignedShort_1_5_5_5_Rev> UnsignedShort_1_5_5_5_Rev;
# endif
#endif
#if defined GL_UNSIGNED_INT_8_8_8_8
# if defined UnsignedInt_8_8_8_8
#  pragma push_macro("UnsignedInt_8_8_8_8")
#  undef UnsignedInt_8_8_8_8
	Transform<PixelDataType::UnsignedInt_8_8_8_8> UnsignedInt_8_8_8_8;
#  pragma pop_macro("UnsignedInt_8_8_8_8")
# else
	Transform<PixelDataType::UnsignedInt_8_8_8_8> UnsignedInt_8_8_8_8;
# endif
#endif
#if defined GL_UNSIGNED_INT_8_8_8_8_REV
# if defined UnsignedInt_8_8_8_8_Rev
#  pragma push_macro("UnsignedInt_8_8_8_8_Rev")
#  undef UnsignedInt_8_8_8_8_Rev
	Transform<PixelDataType::UnsignedInt_8_8_8_8_Rev> UnsignedInt_8_8_8_8_Rev;
#  pragma pop_macro("UnsignedInt_8_8_8_8_Rev")
# else
	Transform<PixelDataType::UnsignedInt_8_8_8_8_Rev> UnsignedInt_8_8_8_8_Rev;
# endif
#endif
#if defined GL_UNSIGNED_INT_10_10_10_2
# if defined UnsignedInt_10_10_10_2
#  pragma push_macro("UnsignedInt_10_10_10_2")
#  undef UnsignedInt_10_10_10_2
	Transform<PixelDataType::UnsignedInt_10_10_10_2> UnsignedInt_10_10_10_2;
#  pragma pop_macro("UnsignedInt_10_10_10_2")
# else
	Transform<PixelDataType::UnsignedInt_10_10_10_2> UnsignedInt_10_10_10_2;
# endif
#endif
#if defined GL_UNSIGNED_INT_2_10_10_10_REV
# if defined UnsignedInt_2_10_10_10_Rev
#  pragma push_macro("UnsignedInt_2_10_10_10_Rev")
#  undef UnsignedInt_2_10_10_10_Rev
	Transform<PixelDataType::UnsignedInt_2_10_10_10_Rev> UnsignedInt_2_10_10_10_Rev;
#  pragma pop_macro("UnsignedInt_2_10_10_10_Rev")
# else
	Transform<PixelDataType::UnsignedInt_2_10_10_10_Rev> UnsignedInt_2_10_10_10_Rev;
# endif
#endif
#if defined GL_UNSIGNED_INT_24_8
# if defined UnsignedInt_24_8
#  pragma push_macro("UnsignedInt_24_8")
#  undef UnsignedInt_24_8
	Transform<PixelDataType::UnsignedInt_24_8> UnsignedInt_24_8;
#  pragma pop_macro("UnsignedInt_24_8")
# else
	Transform<PixelDataType::UnsignedInt_24_8> UnsignedInt_24_8;
# endif
#endif
#if defined GL_UNSIGNED_INT_10F_11F_11F_REV
# if defined UnsignedInt_10f_11f_11f_Rev
#  pragma push_macro("UnsignedInt_10f_11f_11f_Rev")
#  undef UnsignedInt_10f_11f_11f_Rev
	Transform<PixelDataType::UnsignedInt_10f_11f_11f_Rev> UnsignedInt_10f_11f_11f_Rev;
#  pragma pop_macro("UnsignedInt_10f_11f_11f_Rev")
# else
	Transform<PixelDataType::UnsignedInt_10f_11f_11f_Rev> UnsignedInt_10f_11f_11f_Rev;
# endif
#endif
#if defined GL_UNSIGNED_INT_5_9_9_9_REV
# if defined UnsignedInt_5_9_9_9_Rev
#  pragma push_macro("UnsignedInt_5_9_9_9_Rev")
#  undef UnsignedInt_5_9_9_9_Rev
	Transform<PixelDataType::UnsignedInt_5_9_9_9_Rev> UnsignedInt_5_9_9_9_Rev;
#  pragma pop_macro("UnsignedInt_5_9_9_9_Rev")
# else
	Transform<PixelDataType::UnsignedInt_5_9_9_9_Rev> UnsignedInt_5_9_9_9_Rev;
# endif
#endif
#if defined GL_FLOAT_32_UNSIGNED_INT_24_8_REV
# if defined Float_32UnsignedInt_24_8_Rev
#  pragma push_macro("Float_32UnsignedInt_24_8_Rev")
#  undef Float_32UnsignedInt_24_8_Rev
	Transform<PixelDataType::Float_32UnsignedInt_24_8_Rev> Float_32UnsignedInt_24_8_Rev;
#  pragma pop_macro("Float_32UnsignedInt_24_8_Rev")
# else
	Transform<PixelDataType::Float_32UnsignedInt_24_8_Rev> Float_32UnsignedInt_24_8_Rev;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
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
#if defined GL_UNSIGNED_BYTE_3_3_2
# if defined UnsignedByte_3_3_2
#  pragma push_macro("UnsignedByte_3_3_2")
#  undef UnsignedByte_3_3_2
	 , UnsignedByte_3_3_2(_base())
#  pragma pop_macro("UnsignedByte_3_3_2")
# else
	 , UnsignedByte_3_3_2(_base())
# endif
#endif
#if defined GL_UNSIGNED_BYTE_2_3_3_REV
# if defined UnsignedByte_2_3_3_Rev
#  pragma push_macro("UnsignedByte_2_3_3_Rev")
#  undef UnsignedByte_2_3_3_Rev
	 , UnsignedByte_2_3_3_Rev(_base())
#  pragma pop_macro("UnsignedByte_2_3_3_Rev")
# else
	 , UnsignedByte_2_3_3_Rev(_base())
# endif
#endif
#if defined GL_UNSIGNED_SHORT_5_6_5
# if defined UnsignedShort_5_6_5
#  pragma push_macro("UnsignedShort_5_6_5")
#  undef UnsignedShort_5_6_5
	 , UnsignedShort_5_6_5(_base())
#  pragma pop_macro("UnsignedShort_5_6_5")
# else
	 , UnsignedShort_5_6_5(_base())
# endif
#endif
#if defined GL_UNSIGNED_SHORT_5_6_5_REV
# if defined UnsignedShort_5_6_5_Rev
#  pragma push_macro("UnsignedShort_5_6_5_Rev")
#  undef UnsignedShort_5_6_5_Rev
	 , UnsignedShort_5_6_5_Rev(_base())
#  pragma pop_macro("UnsignedShort_5_6_5_Rev")
# else
	 , UnsignedShort_5_6_5_Rev(_base())
# endif
#endif
#if defined GL_UNSIGNED_SHORT_4_4_4_4
# if defined UnsignedShort_4_4_4_4
#  pragma push_macro("UnsignedShort_4_4_4_4")
#  undef UnsignedShort_4_4_4_4
	 , UnsignedShort_4_4_4_4(_base())
#  pragma pop_macro("UnsignedShort_4_4_4_4")
# else
	 , UnsignedShort_4_4_4_4(_base())
# endif
#endif
#if defined GL_UNSIGNED_SHORT_4_4_4_4_REV
# if defined UnsignedShort_4_4_4_4_Rev
#  pragma push_macro("UnsignedShort_4_4_4_4_Rev")
#  undef UnsignedShort_4_4_4_4_Rev
	 , UnsignedShort_4_4_4_4_Rev(_base())
#  pragma pop_macro("UnsignedShort_4_4_4_4_Rev")
# else
	 , UnsignedShort_4_4_4_4_Rev(_base())
# endif
#endif
#if defined GL_UNSIGNED_SHORT_5_5_5_1
# if defined UnsignedShort_5_5_5_1
#  pragma push_macro("UnsignedShort_5_5_5_1")
#  undef UnsignedShort_5_5_5_1
	 , UnsignedShort_5_5_5_1(_base())
#  pragma pop_macro("UnsignedShort_5_5_5_1")
# else
	 , UnsignedShort_5_5_5_1(_base())
# endif
#endif
#if defined GL_UNSIGNED_SHORT_1_5_5_5_REV
# if defined UnsignedShort_1_5_5_5_Rev
#  pragma push_macro("UnsignedShort_1_5_5_5_Rev")
#  undef UnsignedShort_1_5_5_5_Rev
	 , UnsignedShort_1_5_5_5_Rev(_base())
#  pragma pop_macro("UnsignedShort_1_5_5_5_Rev")
# else
	 , UnsignedShort_1_5_5_5_Rev(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_8_8_8_8
# if defined UnsignedInt_8_8_8_8
#  pragma push_macro("UnsignedInt_8_8_8_8")
#  undef UnsignedInt_8_8_8_8
	 , UnsignedInt_8_8_8_8(_base())
#  pragma pop_macro("UnsignedInt_8_8_8_8")
# else
	 , UnsignedInt_8_8_8_8(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_8_8_8_8_REV
# if defined UnsignedInt_8_8_8_8_Rev
#  pragma push_macro("UnsignedInt_8_8_8_8_Rev")
#  undef UnsignedInt_8_8_8_8_Rev
	 , UnsignedInt_8_8_8_8_Rev(_base())
#  pragma pop_macro("UnsignedInt_8_8_8_8_Rev")
# else
	 , UnsignedInt_8_8_8_8_Rev(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_10_10_10_2
# if defined UnsignedInt_10_10_10_2
#  pragma push_macro("UnsignedInt_10_10_10_2")
#  undef UnsignedInt_10_10_10_2
	 , UnsignedInt_10_10_10_2(_base())
#  pragma pop_macro("UnsignedInt_10_10_10_2")
# else
	 , UnsignedInt_10_10_10_2(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_2_10_10_10_REV
# if defined UnsignedInt_2_10_10_10_Rev
#  pragma push_macro("UnsignedInt_2_10_10_10_Rev")
#  undef UnsignedInt_2_10_10_10_Rev
	 , UnsignedInt_2_10_10_10_Rev(_base())
#  pragma pop_macro("UnsignedInt_2_10_10_10_Rev")
# else
	 , UnsignedInt_2_10_10_10_Rev(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_24_8
# if defined UnsignedInt_24_8
#  pragma push_macro("UnsignedInt_24_8")
#  undef UnsignedInt_24_8
	 , UnsignedInt_24_8(_base())
#  pragma pop_macro("UnsignedInt_24_8")
# else
	 , UnsignedInt_24_8(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_10F_11F_11F_REV
# if defined UnsignedInt_10f_11f_11f_Rev
#  pragma push_macro("UnsignedInt_10f_11f_11f_Rev")
#  undef UnsignedInt_10f_11f_11f_Rev
	 , UnsignedInt_10f_11f_11f_Rev(_base())
#  pragma pop_macro("UnsignedInt_10f_11f_11f_Rev")
# else
	 , UnsignedInt_10f_11f_11f_Rev(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_5_9_9_9_REV
# if defined UnsignedInt_5_9_9_9_Rev
#  pragma push_macro("UnsignedInt_5_9_9_9_Rev")
#  undef UnsignedInt_5_9_9_9_Rev
	 , UnsignedInt_5_9_9_9_Rev(_base())
#  pragma pop_macro("UnsignedInt_5_9_9_9_Rev")
# else
	 , UnsignedInt_5_9_9_9_Rev(_base())
# endif
#endif
#if defined GL_FLOAT_32_UNSIGNED_INT_24_8_REV
# if defined Float_32UnsignedInt_24_8_Rev
#  pragma push_macro("Float_32UnsignedInt_24_8_Rev")
#  undef Float_32UnsignedInt_24_8_Rev
	 , Float_32UnsignedInt_24_8_Rev(_base())
#  pragma pop_macro("Float_32UnsignedInt_24_8_Rev")
# else
	 , Float_32UnsignedInt_24_8_Rev(_base())
# endif
#endif
	{ }
};

} // namespace enums

