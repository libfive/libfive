/**
 *  @file oglplus/math/matrix.hpp
 *  @brief A matrix class
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_MATRIX_1107121519_HPP
#define OGLPLUS_MATRIX_1107121519_HPP

#include <oglplus/config/compiler.hpp>
#include <oglplus/math/vector.hpp>
#include <oglplus/math/angle.hpp>
#include <oglplus/math/quaternion.hpp>

#if !OGLPLUS_NO_INITIALIZER_LISTS
#include <initializer_list>
#endif

#include <algorithm>

#include <cassert>
#include <cstddef>
#include <cmath>

namespace oglplus {

template <typename T, std::size_t Rows, std::size_t Cols>
class Matrix;

#if OGLPLUS_DOCUMENTATION_ONLY || defined(GL_FLOAT)
/// 2x2 float matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLfloat, 2, 2> Mat2f;

/// 2x3 float matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLfloat, 2, 3> Mat2x3f;

/// 2x4 float matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLfloat, 2, 4> Mat2x4f;

/// 3x2 float matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLfloat, 3, 2> Mat3x2f;

/// 3x3 float matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLfloat, 3, 3> Mat3f;

/// 3x4 float matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLfloat, 3, 4> Mat3x4f;

/// 4x2 float matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLfloat, 4, 2> Mat4x2f;

/// 4x3 float matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLfloat, 4, 3> Mat4x3f;

/// 4x4 float matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLfloat, 4, 4> Mat4f;
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || defined(GL_DOUBLE)
/// 2x2 double-precision matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLdouble, 2, 2> Mat2d;

/// 2x3 double-precision matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLdouble, 2, 3> Mat2x3d;

/// 2x4 double-precision matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLdouble, 2, 4> Mat2x4d;

/// 3x2 double-precision matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLdouble, 3, 2> Mat3x2d;

/// 3x3 double-precision matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLdouble, 3, 3> Mat3d;

/// 3x4 double-precision matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLdouble, 3, 4> Mat3x4d;

/// 4x2 double-precision matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLdouble, 4, 2> Mat4x2d;

/// 4x3 double-precision matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLdouble, 4, 3> Mat4x3d;

/// 4x4 double-precision matrix
/**
 *  @ingroup math_utils
 */
typedef Matrix<GLdouble, 4, 4> Mat4d;
#endif

namespace aux {
struct Matrix_spec_ctr_tag { };
} // namespace aux

/// Base template for Matrix
template <typename T, std::size_t Rows, std::size_t Cols>
class Matrix
{
protected:
	union {
		T _data[Rows * Cols];
		T _elem[Rows][Cols];
	} _m;

	typedef aux::Matrix_spec_ctr_tag _spec_ctr;

	struct _op_negate
	{
		const Matrix& a;

		void operator()(Matrix& t) const
		{
			for(std::size_t i=0; i!=Rows; ++i)
			for(std::size_t j=0; j!=Cols; ++j)
				t._m._elem[i][j] = -a._m._elem[i][j];
		}
	};

	struct _op_add
	{
		const Matrix& a;
		const Matrix& b;

		void operator()(Matrix& t) const
		{
			for(std::size_t i=0; i!=Rows; ++i)
			for(std::size_t j=0; j!=Cols; ++j)
				t._m._elem[i][j] =
					a._m._elem[i][j]+
					b._m._elem[i][j];
		}
	};

	struct _op_subtract
	{
		const Matrix& a;
		const Matrix& b;

		void operator()(Matrix& t) const
		{
			for(std::size_t i=0; i!=Rows; ++i)
			for(std::size_t j=0; j!=Cols; ++j)
				t._m._elem[i][j] =
					a._m._elem[i][j]-
					b._m._elem[i][j];
		}
	};

	template <std::size_t N>
	struct _op_multiply
	{
		const Matrix<T, Rows, N>& a;
		const Matrix<T, N, Cols>& b;

		void operator()(Matrix& t) const
		{
			for(std::size_t i=0; i!=Rows; ++i)
			for(std::size_t j=0; j!=Cols; ++j)
			{
				t._m._elem[i][j] = a.At(i, 0)* b.At(0, j);
				for(std::size_t k=1; k!=N; ++k)
				{
					t._m._elem[i][j] +=
						a.At(i, k)*
						b.At(k, j);
				}
			}
		}
	};

	struct _op_mult_c
	{
		const Matrix& a;
		const T& v;

		void operator()(Matrix& t) const
		{
			for(std::size_t i=0; i!=Rows; ++i)
			for(std::size_t j=0; j!=Cols; ++j)
				t._m._elem[i][j] = a._m._elem[i][j] * v;
		}
	};

	struct _op_transpose
	{
		const Matrix<T, Cols, Rows>& a;

		void operator()(Matrix& t) const
		{
			for(std::size_t i=0; i!=Rows; ++i)
			for(std::size_t j=0; j!=Cols; ++j)
				t._m._elem[i][j] = a._m._elem[j][i];
		}
	};

	template <std::size_t I, std::size_t J, std::size_t R, std::size_t C>
	struct _op_extract
	{
		const Matrix& a;

		void operator()(Matrix<T, R, C>& t) const
		{
			static_assert(
				I+R<= Rows,
				"Invalid row for this matrix type"
			);
			static_assert(
				J+C<= Cols ,
				"Invalid column for this matrix type"
			);
			for(std::size_t i=0; i!=R; ++i)
			for(std::size_t j=0; j!=C; ++j)
				t.Set(i, j, a.At(I+i, J+j));
		}
	};

	template <typename U, std::size_t R, std::size_t C>
	struct _op_copy
	{
		const Matrix<U, R, C>& a;

		void operator()(Matrix& t) const
		{
			static_assert(
				Rows <= R,
				"Invalid row for this matrix type"
			);
			static_assert(
				Cols <= C ,
				"Invalid column for this matrix type"
			);
			for(std::size_t i=0; i!=Rows; ++i)
			for(std::size_t j=0; j!=Cols; ++j)
				t._m._elem[i][j] = T(a.At(i, j));
		}
	};

	void _init_row(std::size_t r, const T* data, std::size_t cols)
	{
		assert(cols == Cols);
		std::copy(data, data+cols, _m._elem[r]);
	}

	void _init_row(std::size_t r, const Vector<T, Cols>& row)
	{
		std::copy(row.Data(), row.Data()+Cols, _m._elem[r]);
	}

	// No initialization
	Matrix(oglplus::Nothing)
	{ }

public:
	template <typename InitOp>
	explicit Matrix(_spec_ctr, InitOp& init)
	{
		init(*this);
	}

	/// Default construction (identity matrix)
	Matrix(void)
	{
		std::fill(_m._data, _m._data+Rows*Cols, T(0));
		for(std::size_t i=0, n=Rows<Cols?Rows:Cols; i!=n; ++i)
			this->_m._elem[i][i] = T(1);
	}

	/// Constructuion from raw data
	Matrix(const T* data, std::size_t n)
	{
		std::copy(data, data+n, _m._data);
	}

	/// Constructuion from static array
	explicit Matrix(const T (&data)[Rows*Cols])
	{
		std::copy(data, data+Rows*Cols, _m._data);
	}

#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	Matrix(const Matrix&) = default;
	Matrix& operator = (const Matrix&) = default;
#endif

	template <typename U, std::size_t R, std::size_t C>
	explicit Matrix(const Matrix<U, R, C>& other)
	{
		_op_copy<U, R, C> init = {other};
		init(*this);
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Initializing constructor
	/** Allows to explicitly initialize all elements of the matrix.
	 *  The number of parameters of this constructor must be Rows * Cols.
	 *
	 *  @pre (sizeof...(P) == Rows*Cols)
	 */
	template <typename ... P>
	explicit Matrix(P ... p);

	/// Initialization from row vectors
	/** Allows to initialize the matrix from row vectors
	 *  The number of vectors must be Rows, each vector must
	 *  have Cols components.
	 *
	 *  @pre (sizeof...(C) == Rows) && (C == Cols)...
	 */
	template <typename ... C>
	explicit Matrix(const Vector<T, C> ... row);
#elif !OGLPLUS_NO_VARIADIC_TEMPLATES && !OGLPLUS_NO_UNIFIED_INITIALIZATION_SYNTAX

#include <oglplus/math/matrix_n_ctr.ipp>

#else

#include <oglplus/math/matrix_2_ctr.ipp>
#include <oglplus/math/matrix_3_ctr.ipp>
#include <oglplus/math/matrix_4_ctr.ipp>

#endif

	void Fill(T value)
	{
		std::fill(_m._data, _m._data+Rows*Cols, value);
	}

	/// Returns a pointer to the matrix elements in row major order
	const T* Data(void) const
	{
		return this->_m._data;
	}

	/// Returns the number of elements of the matrix
	std::size_t Size(void) const
	{
		return Rows * Cols;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Returns apointer to the matrix elements in row major order
	friend const T* Data(const Matrix& matrix);

	/// Returns the number of elements of the matrix
	friend std::size_t Size(const Matrix& matrix);

	/// Returns the number of rows of the matrix
	friend std::size_t Rows(const Matrix& matrix);

	/// Returns the number of columns of the matrix
	friend std::size_t Cols(const Matrix& matrix);
#endif

	/// Returns the value of the element at position i, j
	/**
	 *  @pre (i < Rows) && (j < Cols)
	 */
	T At(std::size_t i, std::size_t j) const
	{
		assert((i < Rows) && (j < Cols));
		return this->_m._elem[i][j];
	}

	/// Sets the value of the element at position i, j
	/**
	 *  @pre (i < Rows) && (j < Cols)
	 */
	void Set(std::size_t i, std::size_t j, T v)
	{
		assert((i < Rows) && (j < Cols));
		this->_m._elem[i][j] = v;
	}

	/// Returns the i-th row of this matrix
	/**
	 *  @pre (i < Rows)
	 */
	Vector<T, Cols> Row(std::size_t i) const
	{
		assert(i < Rows);
		return Vector<T, Cols>(this->_m._elem[i], Cols);
	}

	/// Return the j-th column of this matrix
	/**
	 *  @pre (j < Cols)
	 */
	Vector<T, Rows> Col(std::size_t j) const
	{
		assert(j < Cols);
		T v[Rows];
		for(std::size_t i=0; i!= Rows; ++i)
			v[i] = this->_m._elem[i][j];
		return Vector<T, Rows>(v, Rows);
	}

	/// Equality comparison function
	friend bool Equal(const Matrix& a, const Matrix& b)
	{
		for(std::size_t i=0; i!=Rows; ++i)
		for(std::size_t j=0; j!=Cols; ++j)
			if(a._m._elem[i][j] != b._m._elem[i][j])
				return false;
		return true;
	}

	/// Equality comparison operator
	friend bool operator == (const Matrix& a, const Matrix& b)
	{
		return Equal(a, b);
	}

	/// Unequality comparison operator
	friend bool operator != (const Matrix& a, const Matrix& b)
	{
		return !Equal(a, b);
	}

	/// Element negation function
	friend Matrix Negated(const Matrix& a)
	{
		_op_negate init = {a};
		return Matrix(_spec_ctr(), init);
	}

	/// Element negation operator
	friend Matrix operator - (const Matrix& a)
	{
		return Negated(a);
	}

	/// Matrix addition
	friend Matrix Added(const Matrix& a, const Matrix& b)
	{
		_op_add init = {a, b};
		return Matrix(_spec_ctr(), init);
	}

	/// Matrix addition operator
	friend Matrix operator + (const Matrix& a, const Matrix& b)
	{
		return Added(a, b);
	}

	/// Matrix subtraction
	friend Matrix Subtracted(const Matrix& a, const Matrix& b)
	{
		_op_subtract init = {a, b};
		return Matrix(_spec_ctr(), init);
	}

	/// Matrix subtraction operator
	friend Matrix operator - (const Matrix& a, const Matrix& b)
	{
		return Subtracted(a, b);
	}

	/// Matrix multiplication
	template <std::size_t N>
	friend Matrix Multiplied(
		const Matrix<T, Rows, N>& a,
		const Matrix<T, N, Cols>& b
	)
	{
		_op_multiply<N> init = {a, b};
		return Matrix(_spec_ctr(), init);
	}

	/// Matrix multiplication operator
	template <std::size_t N>
	friend Matrix operator * (
		const Matrix<T, Rows, N>& a,
		const Matrix<T, N, Cols>& b
	)
	{
		return Multiplied(a, b);
	}

	/// Multiplication by scalar value
	friend Matrix Multiplied(const Matrix& a, T m)
	{
		_op_mult_c init = {a, m};
		return Matrix(_spec_ctr(), init);
	}

	/// Multiplication by scalar value operator
	friend Matrix operator * (const Matrix& a, T m)
	{
		return Multiplied(a, m);
	}

	/// Multiplication by scalar value operator
	friend Matrix operator * (T m, const Matrix& a)
	{
		return Multiplied(a, m);
	}

	/// Matrix transposition
	friend Matrix Transposed(const Matrix<T, Cols, Rows>& a)
	{
		_op_transpose init = {a};
		return Matrix(_spec_ctr(), init);
	}

	/// Submatrix extraction
	template <std::size_t I, std::size_t J, std::size_t R, std::size_t C>
	Matrix<T, R, C> Submatrix(void) const
	{
		_op_extract<I, J, R, C> init = {*this};
		return Matrix<T, R, C>(_spec_ctr(), init);
	}

	/// 2x2 submatrix extraction
	friend Matrix<T, 2, 2> Sub2x2(const Matrix& a)
	{
		_op_extract<0, 0, 2, 2> init = {a};
		return Matrix<T, 2, 2>(_spec_ctr(), init);
	}

	/// 3x3 submatrix extraction
	friend Matrix<T, 3, 3> Sub3x3(const Matrix& a)
	{
		_op_extract<0, 0, 3, 3> init = {a};
		return Matrix<T, 3, 3>(_spec_ctr(), init);
	}

	/// Swaps two rows of the Matrix
	friend void RowSwap(Matrix& m, std::size_t a, std::size_t b)
	{
		assert(a < Rows);
		assert(b < Rows);
		::std::swap_ranges(
			m._m._elem[a],
			m._m._elem[a]+Cols,
			m._m._elem[b]
		);
	}

	/// Multiplies row @a i with coeficient @a k
	friend void RowMultiply(Matrix& m, std::size_t i, T k)
	{
		assert(i < Rows);
		for(std::size_t j=0; j!=Cols; ++j)
			m._m._elem[i][j] *= k;
	}

	/// Adds row @a b multipled by coeficient @a k to row @a a
	friend void RowAdd(Matrix& m, std::size_t a, std::size_t b, T k)
	{
		assert(a < Rows);
		assert(b < Rows);
		for(std::size_t j=0; j!=Cols; ++j)
			m._m._elem[a][j] += m._m._elem[b][j] * k;
	}

	/// The gaussian matrix elimination
	template <std::size_t C>
	friend bool Gauss(Matrix& a, Matrix<T, Rows, C>& b)
	{
		const T zero(0), one(1);
		for(std::size_t i=0; i!=Rows; ++i)
		{
			T d = a._m._elem[i][i];
			if(d == zero)
			{
				for(std::size_t k=i+1; k!=Rows; ++k)
				{
					if(a._m._elem[k][i] != zero)
					{
						RowSwap(a, i, k);
						RowSwap(b, i, k);
						break;
					}
				}
				d = a._m._elem[i][i];
			}
			if(d == zero) return false;

			RowMultiply(a, i, one / d);
			RowMultiply(b, i, one / d);

			for(std::size_t k=i+1; k!=Rows; ++k)
			{
				T c = a._m._elem[k][i];
				if(c != zero)
				{
					RowAdd(a, k, i, -c);
					RowAdd(b, k, i, -c);
				}
			}
		}
		return true;
	}

	/// The Gauss-Jordan matrix elimination
	template <std::size_t C>
	friend bool GaussJordan(Matrix& a, Matrix<T, Rows, C>& b)
	{
		if(!Gauss(a, b)) return false;
		const T zero(0);
		for(std::size_t i=Rows-1; i!=0; --i)
		{
			for(std::size_t k=0; k!=i; ++k)
			{
				T c = a._m._elem[k][i];
				if(c != zero)
				{
					RowAdd(a, k, i, -c);
					RowAdd(b, k, i, -c);
				}
			}
		}
		return true;
	}
};

template <
	std::size_t I,
	std::size_t J,
	std::size_t R,
	std::size_t C,
	typename T,
	std::size_t Rows,
	std::size_t Cols
> inline Matrix<T, R, C> Submatrix(const Matrix<T, Rows, Cols>& a)
{
	return a.template Submatrix<I, J, R, C>();
}

template <typename T, std::size_t R, std::size_t C>
inline bool Close(
	const Matrix<T, R, C>& a,
	const Matrix<T, R, C>& b,
	T eps
)
{
	assert(eps >= T(0));
	for(std::size_t i=0; i!=R; ++i)
	for(std::size_t j=0; j!=C; ++j)
	{
		T u = a.At(i, j);
		T v = b.At(i, j);
		T d = std::abs(u-v);
		bool ca = d <= std::abs(u)*eps;
		bool cb = d <= std::abs(v)*eps;
		if(!ca && !cb) return false;
	}
	return true;
}

template <typename T>
inline void InitMatrix4x4(
	Matrix<T, 4, 4>& matrix,
	T v00, T v01, T v02, T v03,
	T v10, T v11, T v12, T v13,
	T v20, T v21, T v22, T v23,
	T v30, T v31, T v32, T v33
)
{
	matrix.Set(0, 0, v00);
	matrix.Set(0, 1, v01);
	matrix.Set(0, 2, v02);
	matrix.Set(0, 3, v03);

	matrix.Set(1, 0, v10);
	matrix.Set(1, 1, v11);
	matrix.Set(1, 2, v12);
	matrix.Set(1, 3, v13);

	matrix.Set(2, 0, v20);
	matrix.Set(2, 1, v21);
	matrix.Set(2, 2, v22);
	matrix.Set(2, 3, v23);

	matrix.Set(3, 0, v30);
	matrix.Set(3, 1, v31);
	matrix.Set(3, 2, v32);
	matrix.Set(3, 3, v33);
}

template <typename T, std::size_t R, std::size_t C>
inline const T* Data(const Matrix<T, R, C>& matrix)
{
	return matrix.Data();
}

template <typename T, std::size_t R, std::size_t C>
inline std::size_t Size(const Matrix<T, R, C>&)
{
	return R * C;
}

template <typename T, std::size_t R, std::size_t C>
inline std::size_t Rows(const Matrix<T, R, C>&)
{
	return R;
}

template <typename T, std::size_t R, std::size_t C>
inline std::size_t Cols(const Matrix<T, R, C>&)
{
	return C;
}

template <typename T, std::size_t R, std::size_t C>
inline T At(const Matrix<T, R, C>& matrix, std::size_t i, std::size_t j)
{
	return matrix.At(i, j);
}

template <typename T, std::size_t R, std::size_t C>
inline Matrix<T, R, C> Inverse(Matrix<T, R, C> m)
{
	Matrix<T, R, C> i;
	if(!GaussJordan(m, i)) i.Fill(T(0));
	return i;
}

/// Class implementing model transformation matrix named constructors
/** The static member functions of this class can be used to construct
 *  various model transformation matrices.
 *
 *  @ingroup math_utils
 */
template <typename T>
class ModelMatrix
 : public Matrix<T, 4, 4>
{
private:
	typedef Matrix<T, 4, 4> Base;
public:
	/// Constructs an identity matrix
	ModelMatrix(void)
	 : Base()
	{ }

	ModelMatrix(const Base& base)
	 : Base(base)
	{ }

	struct Translation_ { };

	ModelMatrix(Translation_, T dx, T dy, T dz)
	 : Base(oglplus::Nothing())
	{
		InitMatrix4x4(
			*this,
			T(1), T(0), T(0),   dx,
			T(0), T(1), T(0),   dy,
			T(0), T(0), T(1),   dz,
			T(0), T(0), T(0), T(1)
		);
	}

	/// Constructs a translation matrix
	static inline ModelMatrix Translation(T dx, T dy, T dz)
	{
		return ModelMatrix(Translation_(), dx, dy, dz);
	}

	/// Constructs a translation matrix
	static inline ModelMatrix TranslationX(T dx)
	{
		return ModelMatrix(Translation_(), dx, T(0), T(0));
	}

	/// Constructs a translation matrix
	static inline ModelMatrix TranslationY(T dy)
	{
		return ModelMatrix(Translation_(), T(0), dy, T(0));
	}

	/// Constructs a translation matrix
	static inline ModelMatrix TranslationZ(T dz)
	{
		return ModelMatrix(Translation_(), T(0), T(0), dz);
	}

	/// Constructs a translation matrix
	static inline ModelMatrix Translation(const Vector<T, 3>& dp)
	{
		return ModelMatrix(Translation_(), dp.x(), dp.y(), dp.z());
	}

	struct Scale_ { };

	ModelMatrix(Scale_, T sx, T sy, T sz)
	 : Base(oglplus::Nothing())
	{
		InitMatrix4x4(
			*this,
			  sx, T(0), T(0), T(0),
			T(0),   sy, T(0), T(0),
			T(0), T(0),   sz, T(0),
			T(0), T(0), T(0), T(1)
		);
	}

	/// Constructs a scale matrix
	static inline ModelMatrix Scale(T sx, T sy, T sz)
	{
		return ModelMatrix(Scale_(), sx, sy, sz);
	}

	struct Reflection_ { };

	ModelMatrix(Reflection_, bool rx, bool ry, bool rz)
	 : Base(oglplus::Nothing())
	{
		const T _rx = rx ?-T(1):T(1);
		const T _ry = ry ?-T(1):T(1);
		const T _rz = rz ?-T(1):T(1);
		InitMatrix4x4(
			*this,
			 _rx, T(0), T(0), T(0),
			T(0),  _ry, T(0), T(0),
			T(0), T(0),  _rz, T(0),
			T(0), T(0), T(0), T(1)
		);
	}

	/// Constructs a reflection matrix
	static inline ModelMatrix Reflection(bool rx, bool ry, bool rz)
	{
		return ModelMatrix(Reflection_(), rx, ry, rz);
	}

	struct RotationX_ { };

	ModelMatrix(RotationX_, Angle<T> angle)
	 : Base(oglplus::Nothing())
	{
		const T cosx = Cos(angle);
		const T sinx = Sin(angle);
		InitMatrix4x4(
			*this,
			 T(1),  T(0),  T(0),  T(0),
			 T(0),  cosx, -sinx,  T(0),
			 T(0),  sinx,  cosx,  T(0),
			 T(0),  T(0),  T(0),  T(1)
		);
	}

	/// Constructs a X-axis rotation matrix
	static inline ModelMatrix RotationX(Angle<T> angle)
	{
		return ModelMatrix(RotationX_(), angle);
	}

	struct RotationY_ { };

	ModelMatrix(RotationY_, Angle<T> angle)
	 : Base(oglplus::Nothing())
	{
		const T cosx = Cos(angle);
		const T sinx = Sin(angle);
		InitMatrix4x4(
			*this,
			 cosx,  T(0),  sinx,  T(0),
			 T(0),  T(1),  T(0),  T(0),
			-sinx,  T(0),  cosx,  T(0),
			 T(0),  T(0),  T(0),  T(1)
		);
	}

	/// Constructs a Y-axis rotation matrix
	static inline ModelMatrix RotationY(Angle<T> angle)
	{
		return ModelMatrix(RotationY_(), angle);
	}

	struct RotationZ_ { };

	ModelMatrix(RotationZ_, Angle<T> angle)
	 : Base(oglplus::Nothing())
	{
		const T cosx = Cos(angle);
		const T sinx = Sin(angle);
		InitMatrix4x4(
			*this,
			 cosx, -sinx,  T(0),  T(0),
			 sinx,  cosx,  T(0),  T(0),
			 T(0),  T(0),  T(1),  T(0),
			 T(0),  T(0),  T(0),  T(1)
		);
	}

	/// Constructs a Z-axis rotation matrix
	static inline ModelMatrix RotationZ(Angle<T> angle)
	{
		return ModelMatrix(RotationZ_(), angle);
	}

	struct RotationA_ { };

	ModelMatrix(RotationA_, const Vector<T,3>& axis, Angle<T> angle)
	 : Base(oglplus::Nothing())
	{
		const Vector<T, 3> a = Normalized(axis);
		const T sf = Sin(angle);
		const T cf = Cos(angle);
		const T _cf = T(1) - cf;
		const T x = a.At(0), y = a.At(1), z = a.At(2);
		const T xx= x*x, xy= x*y, xz= x*z, yy= y*y, yz= y*z, zz= z*z;
		InitMatrix4x4(
			*this,
			xx*_cf +   cf,  xy*_cf - z*sf,  xz*_cf + y*sf,  T(0),
			xy*_cf + z*sf,  yy*_cf +   cf,  yz*_cf - x*sf,  T(0),
			xz*_cf - y*sf,  yz*_cf + x*sf,  zz*_cf +   cf,  T(0),
			T(0),           T(0),           T(0),           T(1)
		);
	}

	/// Constructs a rotation matrix from a vector and angle
	static inline ModelMatrix RotationA(
		const Vector<T,3>& axis,
		Angle<T> angle
	)
	{
		return ModelMatrix(RotationA_(), axis, angle);
	}

	ModelMatrix(RotationA_, Quaternion<T> quat)
	 : Base(oglplus::Nothing())
	{
		quat.Normalize();
		const T a = quat.At(0);
		const T x = quat.At(1);
		const T y = quat.At(2);
		const T z = quat.At(3);
		InitMatrix4x4(
			*this,
			1-2*y*y-2*z*z,  2*x*y-2*a*z,    2*x*z+2*a*y,    T(0),
			2*x*y+2*a*z,    1-2*x*x-2*z*z,  2*y*z-2*a*x,    T(0),
			2*x*z-2*a*y,    2*y*z+2*a*x,    1-2*x*x-2*y*y,  T(0),
			T(0),           T(0),           T(0),           T(1)
		);
	}

	/// Constructs a rotation matrix from a quaternion
	static inline ModelMatrix RotationQ(const Quaternion<T>& quat)
	{
		return ModelMatrix(RotationA_(), quat);
	}

};

#if OGLPLUS_DOCUMENTATION_ONLY || defined(GL_FLOAT)
/// Model transformation float matrix
/**
 *  @ingroup math_utils
 */
typedef ModelMatrix<GLfloat> ModelMatrixf;
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || defined(GL_DOUBLE)
/// Model transformation double precision matrix
/**
 *  @ingroup math_utils
 */
typedef ModelMatrix<GLdouble> ModelMatrixd;
#endif

/// Class implementing camera matrix named constructors
/** The static methods of this class can be used for the construction
 *  of various camera matrices.
 *
 *  @ingroup math_utils
 */
template <typename T>
class CameraMatrix
 : public Matrix<T, 4, 4>
{
private:
	typedef Matrix<T, 4, 4> Base;
public:

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Constructs an identity matrix
	CameraMatrix(void);
#endif

#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	CameraMatrix(void) = default;
#else
	CameraMatrix(void){ }
#endif

	CameraMatrix(const Base& base)
	 : Base(base)
	{ }

	Vector<T, 3> Position(void) const
	{
		return Vector<T,3>(Inverse(*this).Col(3).Data(), 3);
	}

	Vector<T, 3> Direction(void) const
	{
		return -Vector<T, 3>(this->Row(2).Data(), 3);
	}

	struct Perspective_ { };

	CameraMatrix(
		Perspective_,
		T x_left,
		T x_right,
		T y_bottom,
		T y_top,
		T z_near,
		T z_far
	): Base(oglplus::Nothing())
	{
		T m00 = (T(2) * z_near) / (x_right - x_left);
		T m11 = (T(2) * z_near) / (y_top - y_bottom);
		T m22 = -(z_far + z_near) / (z_far - z_near);

		T m20 = (x_right + x_left) / (x_right - x_left);
		T m21 = (y_top + y_bottom) / (y_top - y_bottom);
		T m23 = -T(1);

		T m32 = -(T(2) * z_far * z_near) / (z_far - z_near);

		InitMatrix4x4(
			*this,
			 m00, T(0),  m20, T(0),
			T(0),  m11,  m21, T(0),
			T(0), T(0),  m22,  m32,
			T(0), T(0),  m23, T(0)
		);
	}

	/// Constructs a perspective projection matrix
	/** Creates a new perspective matrix from @p x_left, @p x_right,
	 *  @p y_botton, @p y_top, @p z_near and @p z_far values.
	 */
	static inline CameraMatrix Perspective(
		T x_left,
		T x_right,
		T y_bottom,
		T y_top,
		T z_near,
		T z_far
	)
	{
		return CameraMatrix(
			Perspective_(),
			x_left,
			x_right,
			y_bottom,
			y_top,
			z_near,
			z_far
		);
	}

	/// Constructs a perspective projection matrix
	/** Creates a new perspective matrix from x-axis @p xfov angle,
	 *  x/y @p aspect ratio and z-axis @p z_near and @p z_far planes
	 */
	static inline CameraMatrix PerspectiveX(
		Angle<T> xfov,
		T aspect,
		T z_near,
		T z_far
	)
	{
		assert(aspect > T(0));
		assert(xfov > Angle<T>::Radians(T(0)));

		T x_right = z_near * Tan(xfov * T(0.5));
		T x_left = -x_right;

		T y_bottom = x_left / aspect;
		T y_top = x_right / aspect;

		return CameraMatrix(
			Perspective_(),
			x_left,
			x_right,
			y_bottom,
			y_top,
			z_near,
			z_far
		);
	}

	static inline CameraMatrix PerspectiveX(
		Angle<T> xfov,
		T width,
		T height,
		T z_near,
		T z_far
	)
	{
		assert(height > 0);
		return PerspectiveX(xfov, width/height, z_near, z_far);
	}

	/// Constructs a perspective projection matrix
	/** Creates a new perspective matrix from y-axis @p yfov angle,
	 *  x/y @p aspect ratio and z-axis @p z_near and @p z_far planes
	 */
	static inline CameraMatrix PerspectiveY(
		Angle<T> yfov,
		T aspect,
		T z_near,
		T z_far
	)
	{
		assert(aspect > T(0));
		assert(yfov > Angle<T>::Radians(T(0)));

		T y_top = z_near * Tan(yfov * T(0.5));
		T y_bottom = -y_top;

		T x_left = y_bottom * aspect;
		T x_right = y_top * aspect;

		return CameraMatrix(
			Perspective_(),
			x_left,
			x_right,
			y_bottom,
			y_top,
			z_near,
			z_far
		);
	}

	static inline CameraMatrix PerspectiveY(
		Angle<T> yfov,
		T width,
		T height,
		T z_near,
		T z_far
	)
	{
		assert(height > 0);
		return PerspectiveY(yfov, width/height, z_near, z_far);
	}

	struct Ortho_ { };

	CameraMatrix(
		Ortho_,
		T x_left,
		T x_right,
		T y_bottom,
		T y_top,
		T z_near,
		T z_far
	): Base(oglplus::Nothing())
	{
		T m00 =  T(2) / (x_right - x_left);
		T m11 =  T(2) / (y_top - y_bottom);
		T m22 = -T(2) / (z_far - z_near);

		T m30 = -(x_right + x_left) / (x_right - x_left);
		T m31 = -(y_top + y_bottom) / (y_top - y_bottom);
		T m32 = -(z_far + z_near)   / (z_far - z_near);

		InitMatrix4x4(
			*this,
			 m00, T(0), T(0),  m30,
			T(0),  m11, T(0),  m31,
			T(0), T(0),  m22,  m32,
			T(0), T(0), T(0), T(1)
		);
	}

	/// Constructs an orthographic projection matrix
	/** Creates a new orthographic matrix from the x-axis @p x_left, @p x_right,
	 *  y-axis @p y_bottom, @p y_top and z-axis @p z_near and @p z_far values
	 */
	static inline CameraMatrix Ortho(
		T x_left,
		T x_right,
		T y_bottom,
		T y_top,
		T z_near,
		T z_far
	)
	{
		return CameraMatrix(
			Ortho_(),
			x_left,
			x_right,
			y_bottom,
			y_top,
			z_near,
			z_far
		);
	}

	/// Constructs an orthographic projection matrix
	/** Creates a new orthographic matrix from x-axis @p width,
	 *  x/y @p aspect ratio and z-axis @p z_near and @p z_far planes
	 */
	static inline CameraMatrix OrthoX(
		T width,
		T aspect,
		T z_near,
		T z_far
	)
	{
		assert(aspect > T(0));
		assert(width > T(0));

		T x_right = width / T(2);
		T x_left = -x_right;

		T y_bottom = x_left / aspect;
		T y_top = x_right / aspect;

		return CameraMatrix(
			Ortho_(),
			x_left,
			x_right,
			y_bottom,
			y_top,
			z_near,
			z_far
		);
	}

	/// Constructs an orthographic projection matrix
	/** Creates a new orthographic matrix from y-axis @p height,
	 *  x/y @p aspect ratio and z-axis @p z_near and @p z_far planes
	 */
	static inline CameraMatrix OrthoY(
		T height,
		T aspect,
		T z_near,
		T z_far
	)
	{
		assert(aspect > T(0));
		assert(height > T(0));

		T y_top = height / T(2);
		T y_bottom = -y_top;

		T x_left = y_bottom * aspect;
		T x_right = y_top * aspect;

		return CameraMatrix(
			Ortho_(),
			x_left,
			x_right,
			y_bottom,
			y_top,
			z_near,
			z_far
		);
	}

	struct ScreenStretch_ { };

	CameraMatrix(
		ScreenStretch_,
		T x_left,
		T x_right,
		T y_bottom,
		T y_top
	): Base(oglplus::Nothing())
	{
		assert((x_right - x_left) != T(0));
		assert((y_top - y_bottom) != T(0));

		T m00 =  T(2) / (x_right - x_left);
		T m11 =  T(2) / (y_top - y_bottom);

		T m30 = -(x_right + x_left) / (x_right - x_left);
		T m31 = -(y_top + y_bottom) / (y_top - y_bottom);

		InitMatrix4x4(
			*this,
			 m00, T(0), T(0),  m30,
			T(0),  m11, T(0),  m31,
			T(0), T(0), T(1), T(0),
			T(0), T(0), T(0), T(1)
		);
	}

	/// Constructs a matrix for stretching NDCs after projection
	/** ScreenStretch constructs a matrix that can be used to stretch
	 *  the normalized device coordinates after projection is applied.
	 */
	static inline CameraMatrix ScreenStretch(
		T x_left,
		T x_right,
		T y_bottom,
		T y_top
	)
	{
		return CameraMatrix(
			ScreenStretch_(),
			x_left,
			x_right,
			y_bottom,
			y_top
		);
	}

	/// Constructs a matrix for stretching NDCs after projection
	/** ScreenTile constructs a matrix, that divides the screen into
	 *  rectangular tiles with the specified divisions and stretches
	 *  the normalized device coordinates to show the specified tile.
	 *
	 *  @pre (x >= 0) && (nx > 0) && (y >= 0) && (ny >= 0)
	 */
	static inline CameraMatrix ScreenTile(
		unsigned x,
		unsigned y,
		unsigned nx,
		unsigned ny
	)
	{
		assert(x < nx);
		assert(y < ny);

		return CameraMatrix(
			ScreenStretch_(),
			-T(1)+T(2*(x+0))/T(nx),
			-T(1)+T(2*(x+1))/T(nx),
			-T(1)+T(2*(y+0))/T(ny),
			-T(1)+T(2*(y+1))/T(ny)
		);
	}

	struct LookingAt_ { };

	CameraMatrix(
		LookingAt_,
		const Vector<T, 3>& eye,
		const Vector<T, 3>& target
	): Base(oglplus::Nothing())
	{
		assert(eye != target);
		Vector<T, 3> z = Normalized(eye - target);
		T zx = z[0];
		T zz = z[2];
		Vector<T, 3> t(zz, T(0), -zx);
		Vector<T, 3> y = Normalized(Cross(z, t));
		Vector<T, 3> x = Cross(y, z);

		InitMatrix4x4(
			*this,
			x[0], x[1], x[2],
			-Dot(eye, x),

			y[0], y[1], y[2],
			-Dot(eye, y),

			z[0], z[1], z[2],
			-Dot(eye, z),

			T(0), T(0), T(0), T(1)
		);
	}

	/// Constructs a 'look-at' matrix from eye and target positions
	static inline CameraMatrix LookingAt(
		const Vector<T, 3>& eye,
		const Vector<T, 3>& target
	)
	{
		return CameraMatrix(LookingAt_(), eye, target);
	}

	CameraMatrix(
		LookingAt_,
		const Vector<T, 3>& eye,
		const Vector<T, 3>& target,
		const Vector<T, 3>& up
	): Base(oglplus::Nothing())
	{
		assert(eye != target);
		Vector<T, 3> z = Normalized(eye - target);
		T dupz = Dot(up, z);
		assert(dupz < 0.9 && dupz >-0.9);
		Vector<T, 3> y = Normalized(
			dupz != 0.0?
			up-z*dupz:
			up
		);
		Vector<T, 3> x = Cross(y, z);

		InitMatrix4x4(
			*this,
			x[0], x[1], x[2],
			-Dot(eye, x),

			y[0], y[1], y[2],
			-Dot(eye, y),

			z[0], z[1], z[2],
			-Dot(eye, z),

			T(0), T(0), T(0), T(1)
		);
	}

	/// Constructs 'look-at' matrix from eye and target positions and up vector
	static inline CameraMatrix LookingAt(
		const Vector<T, 3>& eye,
		const Vector<T, 3>& target,
		const Vector<T, 3>& up
	)
	{
		return CameraMatrix(LookingAt_(), eye, target, up);
	}

	struct Orbiting_ { };

	CameraMatrix(
		Orbiting_,
		const Vector<T, 3>& target,
		T radius,
		Angle<T> azimuth,
		Angle<T> elevation
	)
	{
		Vector<T, 3> z(
			Cos(elevation) * Cos(azimuth),
			Sin(elevation),
			Cos(elevation) *-Sin(azimuth)
		);
		Vector<T, 3> x(
			-Sin(azimuth),
			T(0),
			-Cos(azimuth)
		);
		Vector<T, 3> y = Cross(z, x);

		InitMatrix4x4(
			*this,
			x[0], x[1], x[2],
			Dot(x, z) * -radius - Dot(x, target),

			y[0], y[1], y[2],
			Dot(y, z) * -radius - Dot(y, target),

			z[0], z[1], z[2],
			Dot(z, z) * -radius - Dot(z, target),

			T(0), T(0), T(0), T(1)
		);

	}

	/// Constructs a matrix from target, radius, azimuth and elevation
	static inline CameraMatrix Orbiting(
		const Vector<T, 3>& target,
		T radius,
		Angle<T> azimuth,
		Angle<T> elevation
	)
	{
		return CameraMatrix(
			Orbiting_(),
			target,
			radius,
			azimuth,
			elevation
		);
	}

	struct Pitch_ { };

	CameraMatrix(Pitch_, Angle<T> angle)
	 : Base(oglplus::Nothing())
	{
		const T cosx = Cos(-angle);
		const T sinx = Sin(-angle);
		InitMatrix4x4(
			*this,
			 T(1),  T(0),  T(0),  T(0),
			 T(0),  cosx, -sinx,  T(0),
			 T(0),  sinx,  cosx,  T(0),
			 T(0),  T(0),  T(0),  T(1)
		);
	}

	/// Constructs a X-axis rotation (Pitch/Elevation) matrix
	/** The initial heading is the negative Z-axis, y_top is the Y-axis,
	 *  x_right is X-axis.
	 *  Positive angle values do counter-clockwise rotation (looking up),
	 *  negative angles cause clockwise changes in pitch (looking down).
	 */
	static inline CameraMatrix Pitch(Angle<T> angle)
	{
		return CameraMatrix(Pitch_(), angle);
	}

	struct Yaw_ { };

	CameraMatrix(Yaw_, Angle<T> angle)
	 : Base(oglplus::Nothing())
	{
		const T cosx = Cos(-angle);
		const T sinx = Sin(-angle);
		InitMatrix4x4(
			*this,
			 cosx,  T(0),  sinx,  T(0),
			 T(0),  T(1),  T(0),  T(0),
			-sinx,  T(0),  cosx,  T(0),
			 T(0),  T(0),  T(0),  T(1)
		);
	}

	/// Constructs a Y-axis rotation (Heading/Yaw) matrix
	/** The initial heading is the negative Z-axix, y_top is the Y-axis,
	 *  x_right is X-axis.
	 *  Positive angle values do counter-clockwise rotation, negative
	 *  angles cause clockwise changes in heading.
	 */
	static inline CameraMatrix Yaw(Angle<T> angle)
	{
		return CameraMatrix(Yaw_(), angle);
	}

	struct Roll_ { };

	CameraMatrix(Roll_, Angle<T> angle)
	 : Base(oglplus::Nothing())
	{
		const T cosx = Cos(-angle);
		const T sinx = Sin(-angle);
		InitMatrix4x4(
			*this,
			 cosx, -sinx,  T(0),  T(0),
			 sinx,  cosx,  T(0),  T(0),
			 T(0),  T(0),  T(1),  T(0),
			 T(0),  T(0),  T(0),  T(1)
		);
	}

	/// Constructs a Z-axis rotation (Bank/Roll) matrix
	/** The initial position is that y_top is the Y-axis,
	 *  heading in the negative Z-axis direction, x_right is X-axis.
	 *  Positive angle values do counter-clockwise banking, negative
	 *  angles do clockwise banking.
	 */
	static inline CameraMatrix Roll(Angle<T> angle)
	{
		return CameraMatrix(Roll_(), angle);
	}
};

#if OGLPLUS_DOCUMENTATION_ONLY || defined(GL_FLOAT)
/// Camera matrix using float numbers
/**
 *  @ingroup math_utils
 */
typedef CameraMatrix<GLfloat> CamMatrixf;
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || defined(GL_DOUBLE)
/// Camera matrix using double precition numbers
/**
 *  @ingroup math_utils
 */
typedef CameraMatrix<GLdouble> CamMatrixd;
#endif

} // namespace oglplus

#endif // include guard
