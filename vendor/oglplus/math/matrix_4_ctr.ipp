/**
 *  .file oglplus/math/matrix_4_ctr.ipp
 *  .brief Implementation of 4-row Matrix constructors.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */
	Matrix(
		T v00, T v01,
		T v02, T v03,
		T v04, T v05,
		T v06, T v07
	)
	{
		static_assert(
			(Rows == 4 && Cols == 2) ||
			(Rows == 2 && Cols == 4),
			"Invalid constructor for this matrix type"
		);
		_m._data[ 0] = v00;
		_m._data[ 1] = v01;
		_m._data[ 2] = v02;
		_m._data[ 3] = v03;
		_m._data[ 4] = v04;
		_m._data[ 5] = v05;
		_m._data[ 6] = v06;
		_m._data[ 7] = v07;
	}

	Matrix(
		T v00, T v01, T v02,
		T v03, T v04, T v05,
		T v06, T v07, T v08,
		T v09, T v10, T v11
	)
	{
		static_assert(
			(Rows == 4 && Cols == 3) ||
			(Rows == 3 && Cols == 4),
			"Invalid constructor for this matrix type"
		);
		_m._data[ 0] = v00;
		_m._data[ 1] = v01;
		_m._data[ 2] = v02;
		_m._data[ 3] = v03;
		_m._data[ 4] = v04;
		_m._data[ 5] = v05;
		_m._data[ 6] = v06;
		_m._data[ 7] = v07;
		_m._data[ 8] = v08;
		_m._data[ 9] = v09;
		_m._data[10] = v10;
		_m._data[11] = v11;
	}

	Matrix(
		T v00, T v01, T v02, T v03,
		T v04, T v05, T v06, T v07,
		T v08, T v09, T v10, T v11,
		T v12, T v13, T v14, T v15
	)
	{
		static_assert(
			(Rows == 4 && Cols == 4),
			"Invalid constructor for this matrix type"
		);
		_m._data[ 0] = v00;
		_m._data[ 1] = v01;
		_m._data[ 2] = v02;
		_m._data[ 3] = v03;
		_m._data[ 4] = v04;
		_m._data[ 5] = v05;
		_m._data[ 6] = v06;
		_m._data[ 7] = v07;
		_m._data[ 8] = v08;
		_m._data[ 9] = v09;
		_m._data[10] = v10;
		_m._data[11] = v11;
		_m._data[12] = v12;
		_m._data[13] = v13;
		_m._data[14] = v14;
		_m._data[15] = v15;
	}

	Matrix(
		const Vector<T, Cols>& row0,
		const Vector<T, Cols>& row1,
		const Vector<T, Cols>& row2,
		const Vector<T, Cols>& row3
	)
	{
		this->_init_row(0, row0);
		this->_init_row(1, row1);
		this->_init_row(2, row2);
		this->_init_row(3, row3);
	}

