/**
 *  @file oglplus/math/curve.hpp
 *  @brief Quadratic, cubic and higher-order (bezier) curves and splines
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_MATH_CURVE_1107121519_HPP
#define OGLPLUS_MATH_CURVE_1107121519_HPP

#include <oglplus/math/vector.hpp>
#include <oglplus/math/compile_time.hpp>

#include <vector>
#include <array>
#include <cmath>
#include <cassert>

namespace oglplus {

/// A sequence of Bezier curves, possibly connected at end points
/** This class stores the data for a sequence of connected Bezier curves
 *  of a given @c Order. The begin of the i-th curve (segment) is equal
 *  to the end of the (i-1)-th curve and the end of the i-th curve is
 *  equal to the begin of the (i+1)-th curve. Between the begin and
 *  end (control) points there is a fixed number (Order - 1) of curve control
 *  points. The curves pass through the begin and end and are influenced
 *  by the other control points.
 *
 *  @see CubicBezierLoop
 *
 *  @ingroup math_utils
 */
template <typename Type, typename Parameter, unsigned Order>
class BezierCurves
{
private:
	::std::vector<Type> _points;
	bool _connected;
public:
	static bool Connected(const ::std::vector<Type>& points)
	{
		return ((points.size() - 1) % Order) == 0;
	}

	/// Returns true if the individual curves are connected
	bool Connected(void) const
	{
		return _connected;
	}

	static bool Separated(const ::std::vector<Type>& points)
	{
		return (points.size() % (Order+1)) == 0;
	}

	/// Returns true if the individual curves are connected
	bool Separated(void) const
	{
		return !_connected;
	}

	/// Checks if the sequence of control points is OK for this curve type
	static bool PointsOk(const ::std::vector<Type>& points)
	{
		if(points.empty()) return false;
		return (Connected(points) || Separated(points));
	}

	/// Creates the bezier curves from the control @c points
	/** The number of points must be ((C * Order) + 1) or (C * (Order + 1))
	 *  where @em C is the number of curves (segments) in the sequence.
	 *  If both of the above are true then the curves are considered
	 *  to be connected.
	 */
	BezierCurves(::std::vector<Type>&& points)
	 : _points(std::move(points))
	 , _connected(Connected(_points))
	{
		assert(PointsOk(_points));
	}

	/// Creates the bezier curves from the control @c points
	/** The number of points must be ((C * Order) + 1) and connected
	 *  or (C * (Order + 1)) and not(connected),
	 *  where @em C is the number of curves (segments) in the sequence.
	 */
	BezierCurves(::std::vector<Type>&& points, bool connected)
	 : _points(std::move(points))
	 , _connected(connected)
	{
		assert(PointsOk(_points));
		assert(Connected(_points) == _connected);
	}

	/// Creates the bezier curves from the control @c points
	/** The number of points must be (C * Order + 1) where
	 *  @em C is the number of curves (segments) in the sequence.
	 *  If both of the above are true then the curves are considered
	 *  to be connected.
	 */
	BezierCurves(const ::std::vector<Type>& points)
	 : _points(points)
	 , _connected(Connected(_points))
	{
		assert(PointsOk(_points));
	}

	/// Creates the bezier curves from the control @c points
	/** The number of points must be ((C * Order) + 1) and connected
	 *  or (C * (Order + 1)) and not(connected),
	 *  where @em C is the number of curves (segments) in the sequence.
	 */
	BezierCurves(const ::std::vector<Type>& points, bool connected)
	 : _points(points)
	 , _connected(connected)
	{
		assert(PointsOk(_points));
		assert(Connected(_points) == _connected);
	}

	template <std::size_t N>
	BezierCurves(const ::std::array<Type, N>& points)
	 : _points(points.begin(), points.end())
	 , _connected(Connected(_points))
	{
		assert(PointsOk(_points));
	}

	template <std::size_t N>
	BezierCurves(const ::std::array<Type, N>& points, bool connected)
	 : _points(points.begin(), points.end())
	 , _connected(connected)
	{
		assert(PointsOk(_points));
		assert(Connected(_points) == _connected);
	}

	unsigned SegmentStep(void) const
	{
		assert(PointsOk(_points));

		if(_connected) return Order;
		else return Order+1;
	}

	/// Returns the count of individual curves in the sequence
	unsigned SegmentCount(void) const
	{
		assert(PointsOk(_points));

		if(_connected) return unsigned((_points.size() - 1) / Order);
		else return unsigned(_points.size() / ( Order + 1 ));
	}

	/// Returns the contol points of the curve
	const ::std::vector<Type>& ControlPoints(void) const
	{
		return _points;
	}

	/// Wraps the parameter value to [0.0, 1.0]
	static Parameter Wrap(Parameter t)
	{
		const Parameter zero(0);
		const Parameter one(1);
		if(t < zero) t += ::std::floor(::std::fabs(t))+one;
		else if(t > one) t -= ::std::floor(t);
		assert(t >= zero && t <= one);
		return t;
	}

	/// Gets the point on the curve at position t (must be between 0.0, 1.0)
	Type Position01(Parameter t) const
	{
		const Parameter zero(0);
		const Parameter one(1);

		if(t == one) t = zero;
		assert(t >= zero && t < one);

		Parameter toffs = t*SegmentCount();

		unsigned poffs = unsigned(toffs) * SegmentStep();

		assert(poffs < _points.size() - Order);
		Parameter t_sub = toffs - ::std::floor(toffs);
		return math::Bezier<Type, Parameter, Order>::Position(
			_points.data() + poffs,
			_points.size() - poffs,
			t_sub
		);
	}

	/// Gets the point on the curve at position t wrapped to [0.0, 1.0]
	Type Position(Parameter t) const
	{
		return Position01(Wrap(t));
	}

	/// Makes a sequence of points on the curve (n points per segment)
	void Approximate(std::vector<Type>& dest, unsigned n) const
	{
		unsigned sstep = SegmentStep();
		unsigned s = SegmentCount();

		dest.resize(s*n+1);

		auto p = dest.begin();
		const Parameter t_step = Parameter(1)/n;

		for(unsigned i=0; i!=s; ++i)
		{
			unsigned poffs = i*sstep;
			Parameter t_sub = Parameter(0);
			const Type* data = _points.data() + poffs;
			std::size_t size = _points.size() - poffs;
			for(unsigned j=0; j!=n; ++j)
			{
				typedef math::Bezier<Type, Parameter, Order> b;
				assert(p != dest.end());
				*p = Type(b::Position(data, size, t_sub));
				++p;

				t_sub += t_step;
			}
		}
		assert(p != dest.end());
		*p = _points.back();
		++p;
		assert(p == dest.end());
	}

	/// Returns a sequence of points on the curve (n points per segment)
	::std::vector<Type> Approximate(unsigned n) const
	{
		::std::vector<Type> result;
		Approximate(result, n);
		return result;
	}

	/// Returns a derivative of this curve
	BezierCurves<Type, Parameter, Order-1> Derivative(void) const
	{
		unsigned sstep = SegmentStep();
		unsigned s = SegmentCount();

		::std::vector<Type> new_points(s * Order);
		auto p = new_points.begin();

		for(unsigned i=0; i!=s; ++i)
		{
			for(unsigned j=0; j!=Order; ++j)
			{
				unsigned k = i*sstep+j;
				assert(p != new_points.end());
				*p = (_points[k+1] - _points[k]) * Order;
				++p;
			}
		}
		assert(p == new_points.end());

		return BezierCurves<Type, Parameter, Order-1>(
			std::move(new_points),
			false
		);
	}
};

/// A closed smooth cubic Bezier spline passing through all input points
/** This class constructs a closed sequence of Bezier curves that are smooth
 *  at the curve connection points. The control points between the begin
 *  and end points of each segment are calculated automatically to make
 *  the transition between the individual segments smooth.
 *
 *  @ingroup math_utils
 */
template <typename Type, typename Parameter>
class CubicBezierLoop
 : public BezierCurves<Type, Parameter, 3>
{
private:
	template <typename StdRange>
	static std::vector<Type> _make_cpoints(
		const StdRange& points,
		Parameter r
	)
	{
		std::size_t i = 0, n = points.size();
		assert(n != 0);
		std::vector<Type> result(n * 3 + 1);
		auto ir = result.begin();
		while(i != n)
		{
			unsigned a = unsigned(( n + i - 1 ) % n);
			unsigned b = unsigned(i);
			unsigned c = unsigned(( i + 1 ) % n);
			unsigned d = unsigned(( i + 2 ) % n);
			assert(ir != result.end());
			*ir = points[b];
			++ir;
			assert(ir != result.end());
			*ir = Type(points[b] + (points[c] - points[a])*r);
			++ir;
			assert(ir != result.end());
			*ir = Type(points[c] + (points[b] - points[d])*r);
			++ir;
			++i;
		}
		assert(ir != result.end());
		*ir = points[0]; ++ir;
		assert(ir == result.end());
		return result;
	}
public:
	/// Creates a loop passing through the sequence of the input points
	CubicBezierLoop(
		const ::std::vector<Type>& points,
		Parameter r = Parameter(1)/Parameter(3)
	): BezierCurves<Type, Parameter, 3>(_make_cpoints(points, r))
	{ }

	template <std::size_t N>
	CubicBezierLoop(
		const ::std::array<Type, N>& points,
		Parameter r = Parameter(1)/Parameter(3)
	): BezierCurves<Type, Parameter, 3>(_make_cpoints(points, r))
	{ }
};

} // namespace oglplus

#endif // include guard
