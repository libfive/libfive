/**
 *  .file oglplus/math/vector_1.ipp
 *  .brief Specialization of Vector for 1D vectors
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

template <typename T>
class Vector<T, 1>
 : public VectorBase<T, 1>
{
private:
	typedef VectorBase<T, 1> Base;
	typedef typename Base::Unit_ Unit_;
public:
	Vector(void)
	{ }

	template <typename U, std::size_t M>
	Vector(const Vector<U, M>& vector)
	 : Base(vector)
	{ }

	Vector(const T (&v)[1])
	 : Base(v)
	{ }

	Vector(const T* v, std::size_t n)
	 : Base(v, n)
	{ }

	explicit Vector(T v0)
	 : Base(v0)
	{ }

	template <std::size_t M>
	Vector(const Vector<T, M>& v)
	 : Base(oglplus::Nothing())
	{
		this->_elem[0] = v[0];
	}

	Vector(Unit_, std::size_t axis)
	{
		assert(axis < 1);
		this->_elem[axis] = T(1);
	}

	static Vector Unit(std::size_t axis)
	{
		return Vector(Unit_(), axis);
	}

	T x(void) const
	{
		return this->At(0);
	}

	friend Vector Negated(const Vector& a)
	{
		return Vector(-a[0]);
	}

	friend Vector Added(const Vector& a, const Vector& b)
	{
		return Vector(a[0]+b[0]);
	}

	Vector& operator += (const Vector& v)
	{
		this->Add(v);
		return *this;
	}

	friend Vector Subtracted(const Vector& a, const Vector& b)
	{
		return Vector(a[0]-b[0]);
	}

	Vector& operator -= (const Vector& v)
	{
		this->Subtract(v);
		return *this;
	}


	friend Vector Multiplied(const Vector& a, T v)
	{
		return Vector(a[0]*v);
	}

	Vector& operator *= (T v)
	{
		this->Multiply(v);
		return *this;
	}

	Vector& operator *= (const Vector& v)
	{
		this->Multiply(v);
		return *this;
	}

	friend Vector Divided(const Vector& a, T v)
	{
		return Vector(a[0]/v);
	}

	Vector& operator /= (T v)
	{
		this->Divide(v);
		return *this;
	}
};

