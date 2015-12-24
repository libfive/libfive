/**
 *  .file oglplus/math/matrix_n_ctr.ipp
 *  .brief Implementation of variadic Matrix constructors.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

	template <typename ... P>
	explicit Matrix(T v, P ... p)
	{
		static_assert(
			1 + sizeof...(P) == Rows * Cols,
			"Invalid number of elements for this matrix type"
		);
		T tmp[Rows * Cols] = {v, T(p)...};
		std::copy(tmp, tmp+Rows*Cols, this->_m._data);
	}
private:
	void _init_rows(std::size_t){ }

	template <std::size_t ... C>
	void _init_rows(
		std::size_t i,
		const Vector<T, Cols>& row,
		const Vector<T, C>&... rows
	)
	{
		this->_init_row(i, row);
		this->_init_rows(i+1, rows...);
	}
public:
	template <std::size_t ... C>
	explicit Matrix(const Vector<T, Cols>& row, const Vector<T, C>& ... rows)
	{
		static_assert(
			1 + sizeof...(C) == Rows,
			"Invalid number of rows for this matrix type"
		);
		this->_init_rows(0, row, rows...);
	}

