/*
 *  .file oglplus/math/vector_swizzle.ipp
 *
 *  Automatically generated header file. DO NOT modify manually,
 *  edit 'tools/make_vec_swiz_header.sh' instead.
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

template <typename T, std::size_t N>
class Swizzled_xyzw;

template <typename T>
class Swizzled_xyzw<T, 1>
 : public Vector<T, 1>
{
public:
	Swizzled_xyzw(Vector<T, 1> v)
	 : Vector<T, 1>(v)
	{ }

	T x(void) const  { return this->template At(0); }
};

template <typename T>
class Swizzled_xyzw<T, 2>
 : public Vector<T, 2>
{
public:
	Swizzled_xyzw(Vector<T, 2> v)
	 : Vector<T, 2>(v)
	{ }

	T x(void) const  { return this->template At(0); }
	T y(void) const  { return this->template At(1); }

	Vector<T, 2> xx(void) const { return Vector<T, 2>(x(),x()); }
	Vector<T, 2> xy(void) const { return Vector<T, 2>(x(),y()); }
	Vector<T, 2> yx(void) const { return Vector<T, 2>(y(),x()); }
	Vector<T, 2> yy(void) const { return Vector<T, 2>(y(),y()); }
};

template <typename T>
class Swizzled_xyzw<T, 3>
 : public Vector<T, 3>
{
public:
	Swizzled_xyzw(Vector<T, 3> v)
	 : Vector<T, 3>(v)
	{ }

	T x(void) const  { return this->template At(0); }
	T y(void) const  { return this->template At(1); }
	T z(void) const  { return this->template At(2); }

	Vector<T, 2> xx(void) const { return Vector<T, 2>(x(),x()); }
	Vector<T, 2> xy(void) const { return Vector<T, 2>(x(),y()); }
	Vector<T, 2> xz(void) const { return Vector<T, 2>(x(),z()); }
	Vector<T, 2> yx(void) const { return Vector<T, 2>(y(),x()); }
	Vector<T, 2> yy(void) const { return Vector<T, 2>(y(),y()); }
	Vector<T, 2> yz(void) const { return Vector<T, 2>(y(),z()); }
	Vector<T, 2> zx(void) const { return Vector<T, 2>(z(),x()); }
	Vector<T, 2> zy(void) const { return Vector<T, 2>(z(),y()); }
	Vector<T, 2> zz(void) const { return Vector<T, 2>(z(),z()); }

	Vector<T, 3> xxx(void) const { return Vector<T, 3>(x(),x(),x()); }
	Vector<T, 3> xxy(void) const { return Vector<T, 3>(x(),x(),y()); }
	Vector<T, 3> xxz(void) const { return Vector<T, 3>(x(),x(),z()); }
	Vector<T, 3> xyx(void) const { return Vector<T, 3>(x(),y(),x()); }
	Vector<T, 3> xyy(void) const { return Vector<T, 3>(x(),y(),y()); }
	Vector<T, 3> xyz(void) const { return Vector<T, 3>(x(),y(),z()); }
	Vector<T, 3> xzx(void) const { return Vector<T, 3>(x(),z(),x()); }
	Vector<T, 3> xzy(void) const { return Vector<T, 3>(x(),z(),y()); }
	Vector<T, 3> xzz(void) const { return Vector<T, 3>(x(),z(),z()); }
	Vector<T, 3> yxx(void) const { return Vector<T, 3>(y(),x(),x()); }
	Vector<T, 3> yxy(void) const { return Vector<T, 3>(y(),x(),y()); }
	Vector<T, 3> yxz(void) const { return Vector<T, 3>(y(),x(),z()); }
	Vector<T, 3> yyx(void) const { return Vector<T, 3>(y(),y(),x()); }
	Vector<T, 3> yyy(void) const { return Vector<T, 3>(y(),y(),y()); }
	Vector<T, 3> yyz(void) const { return Vector<T, 3>(y(),y(),z()); }
	Vector<T, 3> yzx(void) const { return Vector<T, 3>(y(),z(),x()); }
	Vector<T, 3> yzy(void) const { return Vector<T, 3>(y(),z(),y()); }
	Vector<T, 3> yzz(void) const { return Vector<T, 3>(y(),z(),z()); }
	Vector<T, 3> zxx(void) const { return Vector<T, 3>(z(),x(),x()); }
	Vector<T, 3> zxy(void) const { return Vector<T, 3>(z(),x(),y()); }
	Vector<T, 3> zxz(void) const { return Vector<T, 3>(z(),x(),z()); }
	Vector<T, 3> zyx(void) const { return Vector<T, 3>(z(),y(),x()); }
	Vector<T, 3> zyy(void) const { return Vector<T, 3>(z(),y(),y()); }
	Vector<T, 3> zyz(void) const { return Vector<T, 3>(z(),y(),z()); }
	Vector<T, 3> zzx(void) const { return Vector<T, 3>(z(),z(),x()); }
	Vector<T, 3> zzy(void) const { return Vector<T, 3>(z(),z(),y()); }
	Vector<T, 3> zzz(void) const { return Vector<T, 3>(z(),z(),z()); }
};

template <typename T>
class Swizzled_xyzw<T, 4>
 : public Vector<T, 4>
{
public:
	Swizzled_xyzw(Vector<T, 4> v)
	 : Vector<T, 4>(v)
	{ }

	T x(void) const  { return this->template At(0); }
	T y(void) const  { return this->template At(1); }
	T z(void) const  { return this->template At(2); }
	T w(void) const  { return this->template At(3); }

	Vector<T, 2> xx(void) const { return Vector<T, 2>(x(),x()); }
	Vector<T, 2> xy(void) const { return Vector<T, 2>(x(),y()); }
	Vector<T, 2> xz(void) const { return Vector<T, 2>(x(),z()); }
	Vector<T, 2> xw(void) const { return Vector<T, 2>(x(),w()); }
	Vector<T, 2> yx(void) const { return Vector<T, 2>(y(),x()); }
	Vector<T, 2> yy(void) const { return Vector<T, 2>(y(),y()); }
	Vector<T, 2> yz(void) const { return Vector<T, 2>(y(),z()); }
	Vector<T, 2> yw(void) const { return Vector<T, 2>(y(),w()); }
	Vector<T, 2> zx(void) const { return Vector<T, 2>(z(),x()); }
	Vector<T, 2> zy(void) const { return Vector<T, 2>(z(),y()); }
	Vector<T, 2> zz(void) const { return Vector<T, 2>(z(),z()); }
	Vector<T, 2> zw(void) const { return Vector<T, 2>(z(),w()); }
	Vector<T, 2> wx(void) const { return Vector<T, 2>(w(),x()); }
	Vector<T, 2> wy(void) const { return Vector<T, 2>(w(),y()); }
	Vector<T, 2> wz(void) const { return Vector<T, 2>(w(),z()); }
	Vector<T, 2> ww(void) const { return Vector<T, 2>(w(),w()); }

	Vector<T, 3> xxx(void) const { return Vector<T, 3>(x(),x(),x()); }
	Vector<T, 3> xxy(void) const { return Vector<T, 3>(x(),x(),y()); }
	Vector<T, 3> xxz(void) const { return Vector<T, 3>(x(),x(),z()); }
	Vector<T, 3> xxw(void) const { return Vector<T, 3>(x(),x(),w()); }
	Vector<T, 3> xyx(void) const { return Vector<T, 3>(x(),y(),x()); }
	Vector<T, 3> xyy(void) const { return Vector<T, 3>(x(),y(),y()); }
	Vector<T, 3> xyz(void) const { return Vector<T, 3>(x(),y(),z()); }
	Vector<T, 3> xyw(void) const { return Vector<T, 3>(x(),y(),w()); }
	Vector<T, 3> xzx(void) const { return Vector<T, 3>(x(),z(),x()); }
	Vector<T, 3> xzy(void) const { return Vector<T, 3>(x(),z(),y()); }
	Vector<T, 3> xzz(void) const { return Vector<T, 3>(x(),z(),z()); }
	Vector<T, 3> xzw(void) const { return Vector<T, 3>(x(),z(),w()); }
	Vector<T, 3> xwx(void) const { return Vector<T, 3>(x(),w(),x()); }
	Vector<T, 3> xwy(void) const { return Vector<T, 3>(x(),w(),y()); }
	Vector<T, 3> xwz(void) const { return Vector<T, 3>(x(),w(),z()); }
	Vector<T, 3> xww(void) const { return Vector<T, 3>(x(),w(),w()); }
	Vector<T, 3> yxx(void) const { return Vector<T, 3>(y(),x(),x()); }
	Vector<T, 3> yxy(void) const { return Vector<T, 3>(y(),x(),y()); }
	Vector<T, 3> yxz(void) const { return Vector<T, 3>(y(),x(),z()); }
	Vector<T, 3> yxw(void) const { return Vector<T, 3>(y(),x(),w()); }
	Vector<T, 3> yyx(void) const { return Vector<T, 3>(y(),y(),x()); }
	Vector<T, 3> yyy(void) const { return Vector<T, 3>(y(),y(),y()); }
	Vector<T, 3> yyz(void) const { return Vector<T, 3>(y(),y(),z()); }
	Vector<T, 3> yyw(void) const { return Vector<T, 3>(y(),y(),w()); }
	Vector<T, 3> yzx(void) const { return Vector<T, 3>(y(),z(),x()); }
	Vector<T, 3> yzy(void) const { return Vector<T, 3>(y(),z(),y()); }
	Vector<T, 3> yzz(void) const { return Vector<T, 3>(y(),z(),z()); }
	Vector<T, 3> yzw(void) const { return Vector<T, 3>(y(),z(),w()); }
	Vector<T, 3> ywx(void) const { return Vector<T, 3>(y(),w(),x()); }
	Vector<T, 3> ywy(void) const { return Vector<T, 3>(y(),w(),y()); }
	Vector<T, 3> ywz(void) const { return Vector<T, 3>(y(),w(),z()); }
	Vector<T, 3> yww(void) const { return Vector<T, 3>(y(),w(),w()); }
	Vector<T, 3> zxx(void) const { return Vector<T, 3>(z(),x(),x()); }
	Vector<T, 3> zxy(void) const { return Vector<T, 3>(z(),x(),y()); }
	Vector<T, 3> zxz(void) const { return Vector<T, 3>(z(),x(),z()); }
	Vector<T, 3> zxw(void) const { return Vector<T, 3>(z(),x(),w()); }
	Vector<T, 3> zyx(void) const { return Vector<T, 3>(z(),y(),x()); }
	Vector<T, 3> zyy(void) const { return Vector<T, 3>(z(),y(),y()); }
	Vector<T, 3> zyz(void) const { return Vector<T, 3>(z(),y(),z()); }
	Vector<T, 3> zyw(void) const { return Vector<T, 3>(z(),y(),w()); }
	Vector<T, 3> zzx(void) const { return Vector<T, 3>(z(),z(),x()); }
	Vector<T, 3> zzy(void) const { return Vector<T, 3>(z(),z(),y()); }
	Vector<T, 3> zzz(void) const { return Vector<T, 3>(z(),z(),z()); }
	Vector<T, 3> zzw(void) const { return Vector<T, 3>(z(),z(),w()); }
	Vector<T, 3> zwx(void) const { return Vector<T, 3>(z(),w(),x()); }
	Vector<T, 3> zwy(void) const { return Vector<T, 3>(z(),w(),y()); }
	Vector<T, 3> zwz(void) const { return Vector<T, 3>(z(),w(),z()); }
	Vector<T, 3> zww(void) const { return Vector<T, 3>(z(),w(),w()); }
	Vector<T, 3> wxx(void) const { return Vector<T, 3>(w(),x(),x()); }
	Vector<T, 3> wxy(void) const { return Vector<T, 3>(w(),x(),y()); }
	Vector<T, 3> wxz(void) const { return Vector<T, 3>(w(),x(),z()); }
	Vector<T, 3> wxw(void) const { return Vector<T, 3>(w(),x(),w()); }
	Vector<T, 3> wyx(void) const { return Vector<T, 3>(w(),y(),x()); }
	Vector<T, 3> wyy(void) const { return Vector<T, 3>(w(),y(),y()); }
	Vector<T, 3> wyz(void) const { return Vector<T, 3>(w(),y(),z()); }
	Vector<T, 3> wyw(void) const { return Vector<T, 3>(w(),y(),w()); }
	Vector<T, 3> wzx(void) const { return Vector<T, 3>(w(),z(),x()); }
	Vector<T, 3> wzy(void) const { return Vector<T, 3>(w(),z(),y()); }
	Vector<T, 3> wzz(void) const { return Vector<T, 3>(w(),z(),z()); }
	Vector<T, 3> wzw(void) const { return Vector<T, 3>(w(),z(),w()); }
	Vector<T, 3> wwx(void) const { return Vector<T, 3>(w(),w(),x()); }
	Vector<T, 3> wwy(void) const { return Vector<T, 3>(w(),w(),y()); }
	Vector<T, 3> wwz(void) const { return Vector<T, 3>(w(),w(),z()); }
	Vector<T, 3> www(void) const { return Vector<T, 3>(w(),w(),w()); }

	Vector<T, 4> xxxx(void) const { return Vector<T, 4>(x(),x(),x(),x()); }
	Vector<T, 4> xxxy(void) const { return Vector<T, 4>(x(),x(),x(),y()); }
	Vector<T, 4> xxxz(void) const { return Vector<T, 4>(x(),x(),x(),z()); }
	Vector<T, 4> xxxw(void) const { return Vector<T, 4>(x(),x(),x(),w()); }
	Vector<T, 4> xxyx(void) const { return Vector<T, 4>(x(),x(),y(),x()); }
	Vector<T, 4> xxyy(void) const { return Vector<T, 4>(x(),x(),y(),y()); }
	Vector<T, 4> xxyz(void) const { return Vector<T, 4>(x(),x(),y(),z()); }
	Vector<T, 4> xxyw(void) const { return Vector<T, 4>(x(),x(),y(),w()); }
	Vector<T, 4> xxzx(void) const { return Vector<T, 4>(x(),x(),z(),x()); }
	Vector<T, 4> xxzy(void) const { return Vector<T, 4>(x(),x(),z(),y()); }
	Vector<T, 4> xxzz(void) const { return Vector<T, 4>(x(),x(),z(),z()); }
	Vector<T, 4> xxzw(void) const { return Vector<T, 4>(x(),x(),z(),w()); }
	Vector<T, 4> xxwx(void) const { return Vector<T, 4>(x(),x(),w(),x()); }
	Vector<T, 4> xxwy(void) const { return Vector<T, 4>(x(),x(),w(),y()); }
	Vector<T, 4> xxwz(void) const { return Vector<T, 4>(x(),x(),w(),z()); }
	Vector<T, 4> xxww(void) const { return Vector<T, 4>(x(),x(),w(),w()); }
	Vector<T, 4> xyxx(void) const { return Vector<T, 4>(x(),y(),x(),x()); }
	Vector<T, 4> xyxy(void) const { return Vector<T, 4>(x(),y(),x(),y()); }
	Vector<T, 4> xyxz(void) const { return Vector<T, 4>(x(),y(),x(),z()); }
	Vector<T, 4> xyxw(void) const { return Vector<T, 4>(x(),y(),x(),w()); }
	Vector<T, 4> xyyx(void) const { return Vector<T, 4>(x(),y(),y(),x()); }
	Vector<T, 4> xyyy(void) const { return Vector<T, 4>(x(),y(),y(),y()); }
	Vector<T, 4> xyyz(void) const { return Vector<T, 4>(x(),y(),y(),z()); }
	Vector<T, 4> xyyw(void) const { return Vector<T, 4>(x(),y(),y(),w()); }
	Vector<T, 4> xyzx(void) const { return Vector<T, 4>(x(),y(),z(),x()); }
	Vector<T, 4> xyzy(void) const { return Vector<T, 4>(x(),y(),z(),y()); }
	Vector<T, 4> xyzz(void) const { return Vector<T, 4>(x(),y(),z(),z()); }
	Vector<T, 4> xyzw(void) const { return Vector<T, 4>(x(),y(),z(),w()); }
	Vector<T, 4> xywx(void) const { return Vector<T, 4>(x(),y(),w(),x()); }
	Vector<T, 4> xywy(void) const { return Vector<T, 4>(x(),y(),w(),y()); }
	Vector<T, 4> xywz(void) const { return Vector<T, 4>(x(),y(),w(),z()); }
	Vector<T, 4> xyww(void) const { return Vector<T, 4>(x(),y(),w(),w()); }
	Vector<T, 4> xzxx(void) const { return Vector<T, 4>(x(),z(),x(),x()); }
	Vector<T, 4> xzxy(void) const { return Vector<T, 4>(x(),z(),x(),y()); }
	Vector<T, 4> xzxz(void) const { return Vector<T, 4>(x(),z(),x(),z()); }
	Vector<T, 4> xzxw(void) const { return Vector<T, 4>(x(),z(),x(),w()); }
	Vector<T, 4> xzyx(void) const { return Vector<T, 4>(x(),z(),y(),x()); }
	Vector<T, 4> xzyy(void) const { return Vector<T, 4>(x(),z(),y(),y()); }
	Vector<T, 4> xzyz(void) const { return Vector<T, 4>(x(),z(),y(),z()); }
	Vector<T, 4> xzyw(void) const { return Vector<T, 4>(x(),z(),y(),w()); }
	Vector<T, 4> xzzx(void) const { return Vector<T, 4>(x(),z(),z(),x()); }
	Vector<T, 4> xzzy(void) const { return Vector<T, 4>(x(),z(),z(),y()); }
	Vector<T, 4> xzzz(void) const { return Vector<T, 4>(x(),z(),z(),z()); }
	Vector<T, 4> xzzw(void) const { return Vector<T, 4>(x(),z(),z(),w()); }
	Vector<T, 4> xzwx(void) const { return Vector<T, 4>(x(),z(),w(),x()); }
	Vector<T, 4> xzwy(void) const { return Vector<T, 4>(x(),z(),w(),y()); }
	Vector<T, 4> xzwz(void) const { return Vector<T, 4>(x(),z(),w(),z()); }
	Vector<T, 4> xzww(void) const { return Vector<T, 4>(x(),z(),w(),w()); }
	Vector<T, 4> xwxx(void) const { return Vector<T, 4>(x(),w(),x(),x()); }
	Vector<T, 4> xwxy(void) const { return Vector<T, 4>(x(),w(),x(),y()); }
	Vector<T, 4> xwxz(void) const { return Vector<T, 4>(x(),w(),x(),z()); }
	Vector<T, 4> xwxw(void) const { return Vector<T, 4>(x(),w(),x(),w()); }
	Vector<T, 4> xwyx(void) const { return Vector<T, 4>(x(),w(),y(),x()); }
	Vector<T, 4> xwyy(void) const { return Vector<T, 4>(x(),w(),y(),y()); }
	Vector<T, 4> xwyz(void) const { return Vector<T, 4>(x(),w(),y(),z()); }
	Vector<T, 4> xwyw(void) const { return Vector<T, 4>(x(),w(),y(),w()); }
	Vector<T, 4> xwzx(void) const { return Vector<T, 4>(x(),w(),z(),x()); }
	Vector<T, 4> xwzy(void) const { return Vector<T, 4>(x(),w(),z(),y()); }
	Vector<T, 4> xwzz(void) const { return Vector<T, 4>(x(),w(),z(),z()); }
	Vector<T, 4> xwzw(void) const { return Vector<T, 4>(x(),w(),z(),w()); }
	Vector<T, 4> xwwx(void) const { return Vector<T, 4>(x(),w(),w(),x()); }
	Vector<T, 4> xwwy(void) const { return Vector<T, 4>(x(),w(),w(),y()); }
	Vector<T, 4> xwwz(void) const { return Vector<T, 4>(x(),w(),w(),z()); }
	Vector<T, 4> xwww(void) const { return Vector<T, 4>(x(),w(),w(),w()); }
	Vector<T, 4> yxxx(void) const { return Vector<T, 4>(y(),x(),x(),x()); }
	Vector<T, 4> yxxy(void) const { return Vector<T, 4>(y(),x(),x(),y()); }
	Vector<T, 4> yxxz(void) const { return Vector<T, 4>(y(),x(),x(),z()); }
	Vector<T, 4> yxxw(void) const { return Vector<T, 4>(y(),x(),x(),w()); }
	Vector<T, 4> yxyx(void) const { return Vector<T, 4>(y(),x(),y(),x()); }
	Vector<T, 4> yxyy(void) const { return Vector<T, 4>(y(),x(),y(),y()); }
	Vector<T, 4> yxyz(void) const { return Vector<T, 4>(y(),x(),y(),z()); }
	Vector<T, 4> yxyw(void) const { return Vector<T, 4>(y(),x(),y(),w()); }
	Vector<T, 4> yxzx(void) const { return Vector<T, 4>(y(),x(),z(),x()); }
	Vector<T, 4> yxzy(void) const { return Vector<T, 4>(y(),x(),z(),y()); }
	Vector<T, 4> yxzz(void) const { return Vector<T, 4>(y(),x(),z(),z()); }
	Vector<T, 4> yxzw(void) const { return Vector<T, 4>(y(),x(),z(),w()); }
	Vector<T, 4> yxwx(void) const { return Vector<T, 4>(y(),x(),w(),x()); }
	Vector<T, 4> yxwy(void) const { return Vector<T, 4>(y(),x(),w(),y()); }
	Vector<T, 4> yxwz(void) const { return Vector<T, 4>(y(),x(),w(),z()); }
	Vector<T, 4> yxww(void) const { return Vector<T, 4>(y(),x(),w(),w()); }
	Vector<T, 4> yyxx(void) const { return Vector<T, 4>(y(),y(),x(),x()); }
	Vector<T, 4> yyxy(void) const { return Vector<T, 4>(y(),y(),x(),y()); }
	Vector<T, 4> yyxz(void) const { return Vector<T, 4>(y(),y(),x(),z()); }
	Vector<T, 4> yyxw(void) const { return Vector<T, 4>(y(),y(),x(),w()); }
	Vector<T, 4> yyyx(void) const { return Vector<T, 4>(y(),y(),y(),x()); }
	Vector<T, 4> yyyy(void) const { return Vector<T, 4>(y(),y(),y(),y()); }
	Vector<T, 4> yyyz(void) const { return Vector<T, 4>(y(),y(),y(),z()); }
	Vector<T, 4> yyyw(void) const { return Vector<T, 4>(y(),y(),y(),w()); }
	Vector<T, 4> yyzx(void) const { return Vector<T, 4>(y(),y(),z(),x()); }
	Vector<T, 4> yyzy(void) const { return Vector<T, 4>(y(),y(),z(),y()); }
	Vector<T, 4> yyzz(void) const { return Vector<T, 4>(y(),y(),z(),z()); }
	Vector<T, 4> yyzw(void) const { return Vector<T, 4>(y(),y(),z(),w()); }
	Vector<T, 4> yywx(void) const { return Vector<T, 4>(y(),y(),w(),x()); }
	Vector<T, 4> yywy(void) const { return Vector<T, 4>(y(),y(),w(),y()); }
	Vector<T, 4> yywz(void) const { return Vector<T, 4>(y(),y(),w(),z()); }
	Vector<T, 4> yyww(void) const { return Vector<T, 4>(y(),y(),w(),w()); }
	Vector<T, 4> yzxx(void) const { return Vector<T, 4>(y(),z(),x(),x()); }
	Vector<T, 4> yzxy(void) const { return Vector<T, 4>(y(),z(),x(),y()); }
	Vector<T, 4> yzxz(void) const { return Vector<T, 4>(y(),z(),x(),z()); }
	Vector<T, 4> yzxw(void) const { return Vector<T, 4>(y(),z(),x(),w()); }
	Vector<T, 4> yzyx(void) const { return Vector<T, 4>(y(),z(),y(),x()); }
	Vector<T, 4> yzyy(void) const { return Vector<T, 4>(y(),z(),y(),y()); }
	Vector<T, 4> yzyz(void) const { return Vector<T, 4>(y(),z(),y(),z()); }
	Vector<T, 4> yzyw(void) const { return Vector<T, 4>(y(),z(),y(),w()); }
	Vector<T, 4> yzzx(void) const { return Vector<T, 4>(y(),z(),z(),x()); }
	Vector<T, 4> yzzy(void) const { return Vector<T, 4>(y(),z(),z(),y()); }
	Vector<T, 4> yzzz(void) const { return Vector<T, 4>(y(),z(),z(),z()); }
	Vector<T, 4> yzzw(void) const { return Vector<T, 4>(y(),z(),z(),w()); }
	Vector<T, 4> yzwx(void) const { return Vector<T, 4>(y(),z(),w(),x()); }
	Vector<T, 4> yzwy(void) const { return Vector<T, 4>(y(),z(),w(),y()); }
	Vector<T, 4> yzwz(void) const { return Vector<T, 4>(y(),z(),w(),z()); }
	Vector<T, 4> yzww(void) const { return Vector<T, 4>(y(),z(),w(),w()); }
	Vector<T, 4> ywxx(void) const { return Vector<T, 4>(y(),w(),x(),x()); }
	Vector<T, 4> ywxy(void) const { return Vector<T, 4>(y(),w(),x(),y()); }
	Vector<T, 4> ywxz(void) const { return Vector<T, 4>(y(),w(),x(),z()); }
	Vector<T, 4> ywxw(void) const { return Vector<T, 4>(y(),w(),x(),w()); }
	Vector<T, 4> ywyx(void) const { return Vector<T, 4>(y(),w(),y(),x()); }
	Vector<T, 4> ywyy(void) const { return Vector<T, 4>(y(),w(),y(),y()); }
	Vector<T, 4> ywyz(void) const { return Vector<T, 4>(y(),w(),y(),z()); }
	Vector<T, 4> ywyw(void) const { return Vector<T, 4>(y(),w(),y(),w()); }
	Vector<T, 4> ywzx(void) const { return Vector<T, 4>(y(),w(),z(),x()); }
	Vector<T, 4> ywzy(void) const { return Vector<T, 4>(y(),w(),z(),y()); }
	Vector<T, 4> ywzz(void) const { return Vector<T, 4>(y(),w(),z(),z()); }
	Vector<T, 4> ywzw(void) const { return Vector<T, 4>(y(),w(),z(),w()); }
	Vector<T, 4> ywwx(void) const { return Vector<T, 4>(y(),w(),w(),x()); }
	Vector<T, 4> ywwy(void) const { return Vector<T, 4>(y(),w(),w(),y()); }
	Vector<T, 4> ywwz(void) const { return Vector<T, 4>(y(),w(),w(),z()); }
	Vector<T, 4> ywww(void) const { return Vector<T, 4>(y(),w(),w(),w()); }
	Vector<T, 4> zxxx(void) const { return Vector<T, 4>(z(),x(),x(),x()); }
	Vector<T, 4> zxxy(void) const { return Vector<T, 4>(z(),x(),x(),y()); }
	Vector<T, 4> zxxz(void) const { return Vector<T, 4>(z(),x(),x(),z()); }
	Vector<T, 4> zxxw(void) const { return Vector<T, 4>(z(),x(),x(),w()); }
	Vector<T, 4> zxyx(void) const { return Vector<T, 4>(z(),x(),y(),x()); }
	Vector<T, 4> zxyy(void) const { return Vector<T, 4>(z(),x(),y(),y()); }
	Vector<T, 4> zxyz(void) const { return Vector<T, 4>(z(),x(),y(),z()); }
	Vector<T, 4> zxyw(void) const { return Vector<T, 4>(z(),x(),y(),w()); }
	Vector<T, 4> zxzx(void) const { return Vector<T, 4>(z(),x(),z(),x()); }
	Vector<T, 4> zxzy(void) const { return Vector<T, 4>(z(),x(),z(),y()); }
	Vector<T, 4> zxzz(void) const { return Vector<T, 4>(z(),x(),z(),z()); }
	Vector<T, 4> zxzw(void) const { return Vector<T, 4>(z(),x(),z(),w()); }
	Vector<T, 4> zxwx(void) const { return Vector<T, 4>(z(),x(),w(),x()); }
	Vector<T, 4> zxwy(void) const { return Vector<T, 4>(z(),x(),w(),y()); }
	Vector<T, 4> zxwz(void) const { return Vector<T, 4>(z(),x(),w(),z()); }
	Vector<T, 4> zxww(void) const { return Vector<T, 4>(z(),x(),w(),w()); }
	Vector<T, 4> zyxx(void) const { return Vector<T, 4>(z(),y(),x(),x()); }
	Vector<T, 4> zyxy(void) const { return Vector<T, 4>(z(),y(),x(),y()); }
	Vector<T, 4> zyxz(void) const { return Vector<T, 4>(z(),y(),x(),z()); }
	Vector<T, 4> zyxw(void) const { return Vector<T, 4>(z(),y(),x(),w()); }
	Vector<T, 4> zyyx(void) const { return Vector<T, 4>(z(),y(),y(),x()); }
	Vector<T, 4> zyyy(void) const { return Vector<T, 4>(z(),y(),y(),y()); }
	Vector<T, 4> zyyz(void) const { return Vector<T, 4>(z(),y(),y(),z()); }
	Vector<T, 4> zyyw(void) const { return Vector<T, 4>(z(),y(),y(),w()); }
	Vector<T, 4> zyzx(void) const { return Vector<T, 4>(z(),y(),z(),x()); }
	Vector<T, 4> zyzy(void) const { return Vector<T, 4>(z(),y(),z(),y()); }
	Vector<T, 4> zyzz(void) const { return Vector<T, 4>(z(),y(),z(),z()); }
	Vector<T, 4> zyzw(void) const { return Vector<T, 4>(z(),y(),z(),w()); }
	Vector<T, 4> zywx(void) const { return Vector<T, 4>(z(),y(),w(),x()); }
	Vector<T, 4> zywy(void) const { return Vector<T, 4>(z(),y(),w(),y()); }
	Vector<T, 4> zywz(void) const { return Vector<T, 4>(z(),y(),w(),z()); }
	Vector<T, 4> zyww(void) const { return Vector<T, 4>(z(),y(),w(),w()); }
	Vector<T, 4> zzxx(void) const { return Vector<T, 4>(z(),z(),x(),x()); }
	Vector<T, 4> zzxy(void) const { return Vector<T, 4>(z(),z(),x(),y()); }
	Vector<T, 4> zzxz(void) const { return Vector<T, 4>(z(),z(),x(),z()); }
	Vector<T, 4> zzxw(void) const { return Vector<T, 4>(z(),z(),x(),w()); }
	Vector<T, 4> zzyx(void) const { return Vector<T, 4>(z(),z(),y(),x()); }
	Vector<T, 4> zzyy(void) const { return Vector<T, 4>(z(),z(),y(),y()); }
	Vector<T, 4> zzyz(void) const { return Vector<T, 4>(z(),z(),y(),z()); }
	Vector<T, 4> zzyw(void) const { return Vector<T, 4>(z(),z(),y(),w()); }
	Vector<T, 4> zzzx(void) const { return Vector<T, 4>(z(),z(),z(),x()); }
	Vector<T, 4> zzzy(void) const { return Vector<T, 4>(z(),z(),z(),y()); }
	Vector<T, 4> zzzz(void) const { return Vector<T, 4>(z(),z(),z(),z()); }
	Vector<T, 4> zzzw(void) const { return Vector<T, 4>(z(),z(),z(),w()); }
	Vector<T, 4> zzwx(void) const { return Vector<T, 4>(z(),z(),w(),x()); }
	Vector<T, 4> zzwy(void) const { return Vector<T, 4>(z(),z(),w(),y()); }
	Vector<T, 4> zzwz(void) const { return Vector<T, 4>(z(),z(),w(),z()); }
	Vector<T, 4> zzww(void) const { return Vector<T, 4>(z(),z(),w(),w()); }
	Vector<T, 4> zwxx(void) const { return Vector<T, 4>(z(),w(),x(),x()); }
	Vector<T, 4> zwxy(void) const { return Vector<T, 4>(z(),w(),x(),y()); }
	Vector<T, 4> zwxz(void) const { return Vector<T, 4>(z(),w(),x(),z()); }
	Vector<T, 4> zwxw(void) const { return Vector<T, 4>(z(),w(),x(),w()); }
	Vector<T, 4> zwyx(void) const { return Vector<T, 4>(z(),w(),y(),x()); }
	Vector<T, 4> zwyy(void) const { return Vector<T, 4>(z(),w(),y(),y()); }
	Vector<T, 4> zwyz(void) const { return Vector<T, 4>(z(),w(),y(),z()); }
	Vector<T, 4> zwyw(void) const { return Vector<T, 4>(z(),w(),y(),w()); }
	Vector<T, 4> zwzx(void) const { return Vector<T, 4>(z(),w(),z(),x()); }
	Vector<T, 4> zwzy(void) const { return Vector<T, 4>(z(),w(),z(),y()); }
	Vector<T, 4> zwzz(void) const { return Vector<T, 4>(z(),w(),z(),z()); }
	Vector<T, 4> zwzw(void) const { return Vector<T, 4>(z(),w(),z(),w()); }
	Vector<T, 4> zwwx(void) const { return Vector<T, 4>(z(),w(),w(),x()); }
	Vector<T, 4> zwwy(void) const { return Vector<T, 4>(z(),w(),w(),y()); }
	Vector<T, 4> zwwz(void) const { return Vector<T, 4>(z(),w(),w(),z()); }
	Vector<T, 4> zwww(void) const { return Vector<T, 4>(z(),w(),w(),w()); }
	Vector<T, 4> wxxx(void) const { return Vector<T, 4>(w(),x(),x(),x()); }
	Vector<T, 4> wxxy(void) const { return Vector<T, 4>(w(),x(),x(),y()); }
	Vector<T, 4> wxxz(void) const { return Vector<T, 4>(w(),x(),x(),z()); }
	Vector<T, 4> wxxw(void) const { return Vector<T, 4>(w(),x(),x(),w()); }
	Vector<T, 4> wxyx(void) const { return Vector<T, 4>(w(),x(),y(),x()); }
	Vector<T, 4> wxyy(void) const { return Vector<T, 4>(w(),x(),y(),y()); }
	Vector<T, 4> wxyz(void) const { return Vector<T, 4>(w(),x(),y(),z()); }
	Vector<T, 4> wxyw(void) const { return Vector<T, 4>(w(),x(),y(),w()); }
	Vector<T, 4> wxzx(void) const { return Vector<T, 4>(w(),x(),z(),x()); }
	Vector<T, 4> wxzy(void) const { return Vector<T, 4>(w(),x(),z(),y()); }
	Vector<T, 4> wxzz(void) const { return Vector<T, 4>(w(),x(),z(),z()); }
	Vector<T, 4> wxzw(void) const { return Vector<T, 4>(w(),x(),z(),w()); }
	Vector<T, 4> wxwx(void) const { return Vector<T, 4>(w(),x(),w(),x()); }
	Vector<T, 4> wxwy(void) const { return Vector<T, 4>(w(),x(),w(),y()); }
	Vector<T, 4> wxwz(void) const { return Vector<T, 4>(w(),x(),w(),z()); }
	Vector<T, 4> wxww(void) const { return Vector<T, 4>(w(),x(),w(),w()); }
	Vector<T, 4> wyxx(void) const { return Vector<T, 4>(w(),y(),x(),x()); }
	Vector<T, 4> wyxy(void) const { return Vector<T, 4>(w(),y(),x(),y()); }
	Vector<T, 4> wyxz(void) const { return Vector<T, 4>(w(),y(),x(),z()); }
	Vector<T, 4> wyxw(void) const { return Vector<T, 4>(w(),y(),x(),w()); }
	Vector<T, 4> wyyx(void) const { return Vector<T, 4>(w(),y(),y(),x()); }
	Vector<T, 4> wyyy(void) const { return Vector<T, 4>(w(),y(),y(),y()); }
	Vector<T, 4> wyyz(void) const { return Vector<T, 4>(w(),y(),y(),z()); }
	Vector<T, 4> wyyw(void) const { return Vector<T, 4>(w(),y(),y(),w()); }
	Vector<T, 4> wyzx(void) const { return Vector<T, 4>(w(),y(),z(),x()); }
	Vector<T, 4> wyzy(void) const { return Vector<T, 4>(w(),y(),z(),y()); }
	Vector<T, 4> wyzz(void) const { return Vector<T, 4>(w(),y(),z(),z()); }
	Vector<T, 4> wyzw(void) const { return Vector<T, 4>(w(),y(),z(),w()); }
	Vector<T, 4> wywx(void) const { return Vector<T, 4>(w(),y(),w(),x()); }
	Vector<T, 4> wywy(void) const { return Vector<T, 4>(w(),y(),w(),y()); }
	Vector<T, 4> wywz(void) const { return Vector<T, 4>(w(),y(),w(),z()); }
	Vector<T, 4> wyww(void) const { return Vector<T, 4>(w(),y(),w(),w()); }
	Vector<T, 4> wzxx(void) const { return Vector<T, 4>(w(),z(),x(),x()); }
	Vector<T, 4> wzxy(void) const { return Vector<T, 4>(w(),z(),x(),y()); }
	Vector<T, 4> wzxz(void) const { return Vector<T, 4>(w(),z(),x(),z()); }
	Vector<T, 4> wzxw(void) const { return Vector<T, 4>(w(),z(),x(),w()); }
	Vector<T, 4> wzyx(void) const { return Vector<T, 4>(w(),z(),y(),x()); }
	Vector<T, 4> wzyy(void) const { return Vector<T, 4>(w(),z(),y(),y()); }
	Vector<T, 4> wzyz(void) const { return Vector<T, 4>(w(),z(),y(),z()); }
	Vector<T, 4> wzyw(void) const { return Vector<T, 4>(w(),z(),y(),w()); }
	Vector<T, 4> wzzx(void) const { return Vector<T, 4>(w(),z(),z(),x()); }
	Vector<T, 4> wzzy(void) const { return Vector<T, 4>(w(),z(),z(),y()); }
	Vector<T, 4> wzzz(void) const { return Vector<T, 4>(w(),z(),z(),z()); }
	Vector<T, 4> wzzw(void) const { return Vector<T, 4>(w(),z(),z(),w()); }
	Vector<T, 4> wzwx(void) const { return Vector<T, 4>(w(),z(),w(),x()); }
	Vector<T, 4> wzwy(void) const { return Vector<T, 4>(w(),z(),w(),y()); }
	Vector<T, 4> wzwz(void) const { return Vector<T, 4>(w(),z(),w(),z()); }
	Vector<T, 4> wzww(void) const { return Vector<T, 4>(w(),z(),w(),w()); }
	Vector<T, 4> wwxx(void) const { return Vector<T, 4>(w(),w(),x(),x()); }
	Vector<T, 4> wwxy(void) const { return Vector<T, 4>(w(),w(),x(),y()); }
	Vector<T, 4> wwxz(void) const { return Vector<T, 4>(w(),w(),x(),z()); }
	Vector<T, 4> wwxw(void) const { return Vector<T, 4>(w(),w(),x(),w()); }
	Vector<T, 4> wwyx(void) const { return Vector<T, 4>(w(),w(),y(),x()); }
	Vector<T, 4> wwyy(void) const { return Vector<T, 4>(w(),w(),y(),y()); }
	Vector<T, 4> wwyz(void) const { return Vector<T, 4>(w(),w(),y(),z()); }
	Vector<T, 4> wwyw(void) const { return Vector<T, 4>(w(),w(),y(),w()); }
	Vector<T, 4> wwzx(void) const { return Vector<T, 4>(w(),w(),z(),x()); }
	Vector<T, 4> wwzy(void) const { return Vector<T, 4>(w(),w(),z(),y()); }
	Vector<T, 4> wwzz(void) const { return Vector<T, 4>(w(),w(),z(),z()); }
	Vector<T, 4> wwzw(void) const { return Vector<T, 4>(w(),w(),z(),w()); }
	Vector<T, 4> wwwx(void) const { return Vector<T, 4>(w(),w(),w(),x()); }
	Vector<T, 4> wwwy(void) const { return Vector<T, 4>(w(),w(),w(),y()); }
	Vector<T, 4> wwwz(void) const { return Vector<T, 4>(w(),w(),w(),z()); }
	Vector<T, 4> wwww(void) const { return Vector<T, 4>(w(),w(),w(),w()); }
};

template <typename T, std::size_t N>
inline Swizzled_xyzw<T, N> Swizzle_xyzw(Vector<T, N> v)
{
	return Swizzled_xyzw<T, N>(v);
}

template <typename T, std::size_t N>
inline Swizzled_xyzw<T, N> Swizzle(Vector<T, N> v)
{
	return Swizzled_xyzw<T, N>(v);
}
