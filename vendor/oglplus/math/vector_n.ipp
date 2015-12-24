/**
 *  .file oglplus/math/vector_n.ipp
 *  .brief A generalized vector template
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

/// Basic template for vector types
/**
 *  @tparam T the coordinate value type
 *  @tparam N the dimension of the Vector
 *
 *  @see Matrix
 *
 *  @ingroup math_utils
 */
template <typename T, std::size_t N>
class Vector
 : public VectorBase<T, N>
{
private:
	typedef VectorBase<T, N> Base;
	typedef typename Base::Unit_ Unit_;

	void _init_by_pack(std::size_t) { }

	template <typename ... P>
	void _init_by_pack(std::size_t i, T v, P ... p)
	{
		this->_elem[i] = v;
		_init_by_pack(i+1, p...);
	}

	struct _spec_ctr { };

	template <typename Op>
	Vector(_spec_ctr, const Vector& a, Op init)
	{
		init(*this, a);
	}

	template <typename Op>
	Vector(_spec_ctr, const Vector& a, const Vector& b, Op init)
	{
		init(*this, a, b);
	}

	template <typename Op>
	Vector(_spec_ctr, const Vector& a, T v, Op init)
	{
		init(*this, a, v);
	}

	struct _op_negate
	{
		void operator()(
			Vector& t,
			const Vector& a
		) const
		{
			for(std::size_t i=0; i!=N; ++i)
				t._elem[i] = -a._elem[i];
		}
	};

	struct _op_add
	{
		void operator()(
			Vector& t,
			const Vector& a,
			const Vector& b
		) const
		{
			for(std::size_t i=0; i!=N; ++i)
				t._elem[i] = a._elem[i] + b._elem[i];
		}
	};

	struct _op_subtract
	{
		void operator()(
			Vector& t,
			const Vector& a,
			const Vector& b
		) const
		{
			for(std::size_t i=0; i!=N; ++i)
				t._elem[i] = a._elem[i] - b._elem[i];
		}
	};

	struct _op_mult_c
	{
		void operator()(
			Vector& t,
			const Vector& a,
			T v
		) const
		{
			for(std::size_t i=0; i!=N; ++i)
				t._elem[i] = a._elem[i] * v;
		}
	};

	struct _op_div_c
	{
		void operator()(
			Vector& t,
			const Vector& a,
			T v
		) const
		{
			for(std::size_t i=0; i!=N; ++i)
				t._elem[i] = a._elem[i] / v;
		}
	};
public:
	/// Default construction, initializes all components to zero
	Vector(void)
	{ }

	/// Copy construction from a vector with different element type
	template <typename U, size_t M>
	Vector(const Vector<U, M>& vector)
	 : Base(vector)
	{ }

	/// Initialization from C-array
	Vector(const T (&v)[N])
	 : Base(v)
	{ }

	/// Initialization from pointer and size
	Vector(const T* v, std::size_t n)
	 : Base(v, n)
	{ }

	/// Initialization from poiner, size and a default value
	Vector(const T* v, std::size_t n, T def)
	 : Base(v, n, def)
	{ }

	/// Initialization of all components by @p v
	explicit Vector(T v)
	 : Base(v)
	{ }

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Initialization from N values
	template <typename ... P>
	Vector(P ... vn);
#else
	template <typename ... P>
	Vector(T v0, T v1, P ... vn)
	 : Base(oglplus::Nothing())
	{
		static_assert(
			2+sizeof...(P) == N,
			"Invalid number of arguments!"
		);
		_init_by_pack(0, v0, v1, vn...);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Initialization from a vector of smaller dimension and additional values
	template <typename ... P>
	Vector(const Vector<T, M>& a, P ... p);
#else
	template <typename U, typename ... P>
	Vector(const Vector<U, N-1-sizeof...(P)>& a, T v, P ... p)
	 : Base(oglplus::Nothing())
	{
		const std::size_t M = N-1-sizeof...(P);
		for(std::size_t i=0; i!=M; ++i)
			this->_elem[i] = T(a[i]);
		_init_by_pack(M, v, p...);
	}
#endif

	Vector(Unit_, std::size_t axis)
	{
		assert(axis < N);
		this->_elem[axis] = T(1);
	}

	/// Construction of a unit vector aligned with the specified @p axis
	static Vector Unit(std::size_t axis)
	{
		return Vector(Unit_(), axis);
	}

	/// Construction from to Matrix-1xN
	explicit Vector(const Matrix<T, 1, N>& matrix)
	 : Base(matrix)
	{ }

	/// Construction from to Matrix-Nx1
	explicit Vector(const Matrix<T, N, 1>& matrix)
	 : Base(matrix)
	{ }

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Returns the dimension of the vector
	friend std::size_t Size(const Vector& a);

	/// Returns a pointer to an array containing the vectors coordinates
	friend const T* Data(const Vector& vector);

	/// Returns the value of the i-th coordinate of the vector
	/**
	 *  @param vector the vector to operate on
	 *  @param i the index of the coordinate to be retrieved
	 *  @pre (i < Size())
	 */
	T At(const Vector& vector, std::size_t i) const
#endif

	/// Returns the 0-th component
	T x(void) const
	{
		return this->At(0);
	}

	/// Returns the 1-st component
	T y(void) const
	{
		return this->At(1);
	}

	/// Returns the 2-nd component
	T z(void) const
	{
		return this->At(2);
	}

	/// Returns the 3-nd component
	T w(void) const
	{
		return this->At(3);
	}

	/// Returns a subvector with the first two components
	Vector<T, 2> xy(void) const
	{
		return Vector<T, 2>(this->At(0), this->At(1));
	}

	/// Returns a subvector with the first three components
	Vector<T, 3> xyz(void) const
	{
		return Vector<T, 3>(this->At(0), this->At(1), this->At(2));
	}

	/// Returns a new vector that is a negation of vector @p a
	friend Vector Negated(const Vector& a)
	{
		return Vector(_spec_ctr(), a, _op_negate());
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Identity operator
	friend Vector operator + (const Vector& a);
	/// Negation operator
	friend Vector operator - (const Vector& a);
#endif

	/// Adds two vectors
	friend Vector Added(const Vector& a, const Vector& b)
	{
		return Vector(_spec_ctr(), a, b, _op_add());
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vector addition operator
	friend Vector operator + (const Vector& a, const Vector& b);
#endif

	/// Adds a vector to this vector
	Vector& operator += (const Vector& v)
	{
		this->Add(v);
		return *this;
	}

	/// Subtracts two vectors
	friend Vector Subtracted(const Vector& a, const Vector& b)
	{
		return Vector(_spec_ctr(), a, b, _op_subtract());
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vector subtration operator
	friend Vector operator - (const Vector& a, const Vector& b);
#endif

	/// Subtracts a vector from this vector
	Vector& operator -= (const Vector& v)
	{
		this->Subtract(v);
		return *this;
	}

	/// Multiples a vector by a scalar value
	friend Vector Multiplied(const Vector& a, T v)
	{
		return Vector(_spec_ctr(), a, v, _op_mult_c());
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Operator for multiplication by a scalar value
	friend Vector operator * (const Vector& a, T v);

	/// Operator for multiplication by a scalar value
	friend Vector operator * (T v, const Vector& a);
#endif

	/// Multiples this vector by a scalar value
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

	/// Divides a vector by a scalar value
	friend Vector Divided(const Vector& a, T v)
	{
		return Vector(_spec_ctr(), a, v, _op_div_c());
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Operator for division by a scalar value
	friend Vector operator / (const Vector& a, T v);
#endif

	/// Divides this vector by a scalar value
	Vector& operator /= (T v)
	{
		this->Divide(v);
		return *this;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Dot product of two vectors
	friend T Dot(const Vector& a, const Vector& b);

	/// Returns the length of a vector
	friend T Length(const Vector& a);

	/// Returns the distance between two vectors
	friend T Distance(const Vector& a, const Vector& b);

	/// Returns a normalized version of the vector passed as argument
	friend Vector Normalized(Vector a);

	/// Vector by Matrix multiplication operator
	template <std::size_t Cols>
	friend Vector<T, Cols> operator * (
		const Vector& v,
		const Matrix<T, N, Cols>& m
	);

	/// Matrix by Vector multiplication operator
	template <std::size_t Rows>
	friend Vector<T, Rows> operator * (
		const Matrix<T, Rows, N>& m,
		const Vector& v
	);
#endif
};

