/**
 *  .file oglplus/math/matrix_2_ctr.ipp
 *  .brief Implementation of 2-row Matrix constructors.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */
	Matrix(
		T v00, T v01,
		T v02, T v03
	)
	{
		static_assert(
			(Rows == 2 && Cols == 2),
			"Invalid constructor for this matrix type"
		);
		_m._data[ 0] = v00;
		_m._data[ 1] = v01;
		_m._data[ 2] = v02;
		_m._data[ 3] = v03;
	}

	Matrix(
		const Vector<T, Cols>& row0,
		const Vector<T, Cols>& row1
	)
	{
		this->_init_row(0, row0);
		this->_init_row(1, row1);
	}

