/**
 *  .file oglplus/math/vector_2.ipp
 *  .brief Specialization of Vector for 2D vectors
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

template <typename T>
class Vector<T, 2>
 : public VectorBase<T, 2>
{
private:
	typedef VectorBase<T, 2> Base;
	typedef typename Base::Unit_ Unit_;
public:
	Vector(void)
	{ }

	template <typename U, std::size_t M>
	Vector(const Vector<U, M>& vector)
	 : Base(vector)
	{ }

	Vector(const T (&v)[2])
	 : Base(v)
	{ }

	Vector(const T* v, std::size_t n)
	 : Base(v, n)
	{ }

	explicit Vector(T v0)
	 : Base(v0)
	{ }

	Vector(T v0, T v1)
	 : Base(oglplus::Nothing())
	{
		this->_elem[0] = v0;
		this->_elem[1] = v1;
	}

	template <typename U>
	Vector(const Vector<U, 1>& v, T v1)
	 : Base(oglplus::Nothing())
	{
		this->_elem[0] = T(v[0]);
		this->_elem[1] = v1;
	}

	Vector(Unit_, std::size_t axis)
	{
		assert(axis < 2);
		this->_elem[axis] = T(1);
	}

	static Vector Unit(std::size_t axis)
	{
		return Vector(Unit_(), axis);
	}

	explicit Vector(const Matrix<T, 1, 2>& matrix)
	 : Base(matrix)
	{ }

	explicit Vector(const Matrix<T, 2, 1>& matrix)
	 : Base(matrix)
	{ }

	T x(void) const
	{
		return this->At(0);
	}

	T y(void) const
	{
		return this->At(1);
	}

	friend Vector Negated(const Vector& a)
	{
		return Vector(-a[0], -a[1]);
	}

	friend Vector Added(const Vector& a, const Vector& b)
	{
		return Vector(a[0]+b[0], a[1]+b[1]);
	}

	Vector& operator += (const Vector& v)
	{
		this->Add(v);
		return *this;
	}

	friend Vector Subtracted(const Vector& a, const Vector& b)
	{
		return Vector(a[0]-b[0], a[1]-b[1]);
	}

	Vector& operator -= (const Vector& v)
	{
		this->Subtract(v);
		return *this;
	}


	friend Vector Multiplied(const Vector& a, T v)
	{
		return Vector(a[0]*v, a[1]*v);
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
		return Vector(a[0]/v, a[1]/v);
	}

	Vector& operator /= (T v)
	{
		this->Divide(v);
		return *this;
	}
};

