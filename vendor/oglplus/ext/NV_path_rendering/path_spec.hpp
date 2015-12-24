/**
 *  @file oglplus/ext/NV_path_rendering/path_spec.hpp
 *  @brief Wrapper for the NV_path_rendering path specification wrapper
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_EXT_NV_PATH_RENDERING_PATH_SPEC_1203031902_HPP
#define OGLPLUS_EXT_NV_PATH_RENDERING_PATH_SPEC_1203031902_HPP

#if OGLPLUS_DOCUMENTATION_ONLY || GL_NV_path_rendering

#include <oglplus/ext/NV_path_rendering/command.hpp>

#include <vector>

namespace oglplus {

/// Template class for convenient specifying of path commands and coordinates
/** This function can be used to specify the commands and coordinates of a
 *  PathNV instance, using the @c Spec member function.
 *
 *  @see PathNV
 *  @see PathNVCommand
 */
template <typename PathCoordType>
class PathNVSpec
{
private:
	friend class ObjectOps<tag::DirectState, tag::PathNV>;

	std::vector<PathNVCommand> _commands;
	std::vector<PathCoordType> _coords;

	typedef PathCoordType T;

	PathNVSpec& _append(PathNVCommand command)
	{
		_commands.push_back(command);

		return *this;
	}

	PathNVSpec& _append(PathNVCommand command, T coord)
	{
		_commands.push_back(command);
		_coords.push_back(coord);

		return *this;
	}

	template <typename ... C>
	PathNVSpec& _append(PathNVCommand command, C ... coord)
	{
		const std::size_t N = sizeof ... (coord);
		const T coords[N] = { T(coord)... };

		_commands.push_back(command);
		_coords.insert(_coords.end(), coords, coords+N);

		return *this;
	}
public:
	/// Creates an empty path specification
	PathNVSpec(void) { }

	/// Creates an empty path specification with storage size hints
	/** This constructor pre-allocates space for @p command_count_hint
	 *  commands and @p coord_count_hint coordinate values.
	 */
	PathNVSpec(GLuint command_count_hint, GLuint coord_count_hint)
	{
		_commands.reserve(command_count_hint);
		_coords.reserve(coord_count_hint);
	}

	PathNVSpec(PathNVSpec&& tmp)
	 : _commands(std::move(tmp._commands))
	 , _coords(std::move(tmp._coords))
	{ }

	/** @name Path specification commands
	 *  The path specification is built by calling the member functions
	 *  in this group. Every one of these functions appends a the specified
	 *  command to the end of the command list and for commands with
	 *  coordinates it also appends the specified values to the coordinate
	 *  list.
	 */
	///@{
	/// Close path
	PathNVSpec& Close(void)
	{
		return _append(PathNVCommand::Close);
	}

	/// Restart path
	PathNVSpec& Restart(void)
	{
		return _append(PathNVCommand::Restart);
	}

	/// Move to absolute coordinates
	PathNVSpec& MoveTo(T c0, T c1)
	{
		return _append(PathNVCommand::MoveTo, c0, c1);
	}

	/// Move by specified amount
	PathNVSpec& RelativeMoveTo(T c0, T c1)
	{
		return _append(PathNVCommand::RelativeMoveTo, c0, c1);
	}

	/// Draw a line to absolute coordinates
	PathNVSpec& LineTo(T c0, T c1)
	{
		return _append(PathNVCommand::LineTo, c0, c1);
	}

	/// Draw a line in the specified direction
	PathNVSpec& RelativeLineTo(T c0, T c1)
	{
		return _append(PathNVCommand::RelativeLineTo, c0, c1);
	}

	/// Draw a horizontal line to the specified vertical coordinate
	PathNVSpec& HorizontalLineTo(T c0)
	{
		return _append(PathNVCommand::HorizontalLineTo, c0);
	}

	/// Draw a horizontal line in the specified direction
	PathNVSpec& RelativeHorizontalLineTo(T c0)
	{
		return _append(PathNVCommand::RelativeHorizontalLineTo, c0);
	}

	/// Draw a vertical line to the specified horizontal coordinate
	PathNVSpec& VerticalLineTo(T c0)
	{
		return _append(PathNVCommand::VerticalLineTo, c0);
	}

	/// Draw a vertical line in the specified direction
	PathNVSpec& RelativeVerticalLineTo(T c0)
	{
		return _append(PathNVCommand::RelativeVerticalLineTo, c0);
	}

	/// Draw a quadratic curve with the specified absolute control points
	PathNVSpec& QuadraticCurveTo(T c0, T c1, T c2, T c3)
	{
		return _append(
			PathNVCommand::QuadraticCurveTo,
			c0, c1, c2, c3
		);
	}

	/// Draw a quadratic curve with the specified relative control points
	PathNVSpec& RelativeQuadraticCurveTo(T c0, T c1, T c2, T c3)
	{
		return _append(
			PathNVCommand::RelativeQuadraticCurveTo,
			c0, c1, c2, c3
		);
	}

	/// Draw a cubic curve with the specified absolute control points
	PathNVSpec& CubicCurveTo(T c0, T c1, T c2, T c3, T c4, T c5)
	{
		return _append(
			PathNVCommand::CubicCurveTo,
			c0, c1, c2, c3, c4, c5
		);
	}

	/// Draw a cubic curve with the specified relative control points
	PathNVSpec& RelativeCubicCurveTo(T c0, T c1, T c2, T c3, T c4, T c5)
	{
		return _append(
			PathNVCommand::RelativeCubicCurveTo,
			c0, c1, c2, c3, c4, c5
		);
	}

	/// Draw a smooth quadratic curve with specified absolute control points
	PathNVSpec& SmoothQuadraticCurveTo(T c0, T c1)
	{
		return _append(
			PathNVCommand::SmoothQuadraticCurveTo,
			c0, c1
		);
	}

	/// Draw a smooth quadratic curve with specified relative control points
	PathNVSpec& RelativeSmoothQuadraticCurveTo(T c0, T c1)
	{
		return _append(
			PathNVCommand::RelativeSmoothQuadraticCurveTo,
			c0, c1
		);
	}

	/// Draw a smooth cubic curve with specified absolute control points
	PathNVSpec& SmoothCubicCurveTo(T c0, T c1, T c2, T c3)
	{
		return _append(
			PathNVCommand::SmoothCubicCurveTo,
			c0, c1, c2, c3
		);
	}

	/// Draw a smooth cubic curve with specified relative control points
	PathNVSpec& RelativeSmoothCubicCurveTo(T c0, T c1, T c2, T c3)
	{
		return _append(
			PathNVCommand::RelativeSmoothCubicCurveTo,
			c0, c1, c2, c3
		);
	}

	/// Draw a small-sweep CCW elliptical arc with absolute parameters
	PathNVSpec& SmallCCWArcTo(T c0, T c1, T c2, T c3, T c4)
	{
		return _append(
			PathNVCommand::SmallCCWArcTo,
			c0, c1, c2, c3, c4
		);
	}

	/// Draw a small-sweep CCW elliptical arc with relative parameters
	PathNVSpec& RelativeSmallCCWArcTo(T c0, T c1, T c2, T c3, T c4)
	{
		return _append(
			PathNVCommand::RelativeSmallCCWArcTo,
			c0, c1, c2, c3, c4
		);
	}

	/// Draw a small-sweep CW elliptical arc with absolute parameters
	PathNVSpec& SmallCWArcTo(T c0, T c1, T c2, T c3, T c4)
	{
		return _append(
			PathNVCommand::SmallCWArcTo,
			c0, c1, c2, c3, c4
		);
	}

	/// Draw a small-sweep CW elliptical arc with relative parameters
	PathNVSpec& RelativeSmallCWArcTo(T c0, T c1, T c2, T c3, T c4)
	{
		return _append(
			PathNVCommand::RelativeSmallCWArcTo,
			c0, c1, c2, c3, c4
		);
	}

	/// Draw a large-sweep CCW elliptical arc with absolute parameters
	PathNVSpec& LargeCCWArcTo(T c0, T c1, T c2, T c3, T c4)
	{
		return _append(
			PathNVCommand::LargeCCWArcTo,
			c0, c1, c2, c3, c4
		);
	}

	/// Draw a large-sweep CCW elliptical arc with relative parameters
	PathNVSpec& RelativeLargeCCWArcTo(T c0, T c1, T c2, T c3, T c4)
	{
		return _append(
			PathNVCommand::RelativeLargeCCWArcTo,
			c0, c1, c2, c3, c4
		);
	}

	/// Draw a large-sweep CW elliptical arc with absolute parameters
	PathNVSpec& LargeCWArcTo(T c0, T c1, T c2, T c3, T c4)
	{
		return _append(
			PathNVCommand::LargeCWArcTo,
			c0, c1, c2, c3, c4
		);
	}

	/// Draw a large-sweep CW elliptical arc with relative parameters
	PathNVSpec& RelativeLargeCWArcTo(T c0, T c1, T c2, T c3, T c4)
	{
		return _append(
			PathNVCommand::RelativeLargeCWArcTo,
			c0, c1, c2, c3, c4
		);
	}

	/// Draw a cubic curve segment duplicating first control point
	PathNVSpec& DupFirstCubicCurveTo(T c0, T c1, T c2, T c3)
	{
		return _append(
			PathNVCommand::DupFirstCubicCurveTo,
			c0, c1, c2, c3
		);
	}

	/// Draw a cubic curve segment duplicating last control point
	PathNVSpec& DupLastCubicCurveTo(T c0, T c1, T c2, T c3)
	{
		return _append(
			PathNVCommand::DupLastCubicCurveTo,
			c0, c1, c2, c3
		);
	}

	/// Draw a rectangle with the specified absolute coordinates
	PathNVSpec& Rect(T c0, T c1, T c2, T c3)
	{
		return _append(PathNVCommand::Rect, c0, c1, c2, c3);
	}

	/// Draw a circular CCW arc with the specified absolute coordinates
	PathNVSpec& CircularCCWArcTo(T c0, T c1, T c2, T c3, T c4)
	{
		return _append(
			PathNVCommand::CircularCCWArcTo,
			c0, c1, c2, c3, c4
		);
	}

	/// Draw a circular CW arc with the specified absolute coordinates
	PathNVSpec& CircularCWArcTo(T c0, T c1, T c2, T c3, T c4)
	{
		return _append(
			PathNVCommand::CircularCWArcTo,
			c0, c1, c2, c3, c4
		);
	}

	/// Draw a circular tangential arc with specified absolute coordinates
	PathNVSpec& CircularTangentArcTo(T c0, T c1, T c2, T c3, T c4)
	{
		return _append(
			PathNVCommand::CircularTangentArcTo,
			c0, c1, c2, c3, c4
		);
	}

	/// Draw a general elliptical arc with specified absolute coordinates
	PathNVSpec& ArcTo(T c0, T c1, T c2, T c3, T c4, T c5, T c6)
	{
		return _append(
			PathNVCommand::ArcTo,
			c0, c1, c2, c3, c4, c5, c6
		);
	}

	/// Draw a general elliptical arc with specified relative coordinates
	PathNVSpec& RelativeArcTo(T c0, T c1, T c2, T c3, T c4, T c5, T c6)
	{
		return _append(
			PathNVCommand::RelativeArcTo,
			c0, c1, c2, c3, c4, c5, c6
		);
	}
	///@}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_NV_path_rendering_shared_edge
	/// Makes the last appended edge of the path a shared edge.
	PathNVSpec& SharedEdge(void)
	{
		if(!_commands.empty())
		{
			_commands.back() |= GL_SHARED_EDGE_NV;
		}
		return *this;
	}
#endif
};

/// Facade over PathNVSpec for even more convenient path specification
/**
 *  @see PathNVSpec
 *  @see PathNV
 */
template <typename PathCoordType>
class BriefPathNVSpec
 : public PathNVSpec<PathCoordType>
{
private:
	typedef PathNVSpec<PathCoordType> Base;
	typedef PathCoordType CT;

	BriefPathNVSpec& self(Base& base)
	{
		return static_cast<BriefPathNVSpec&>(base);
	}
public:
	BriefPathNVSpec(void) { }

	BriefPathNVSpec(GLuint command_count_hint, GLuint coord_count_hint)
	 : Base(command_count_hint, coord_count_hint)
	{ }

	/// Close
	BriefPathNVSpec& Z(void)
	{
		return self(this->Close());
	}

	/// MoveTo
	BriefPathNVSpec& M(CT c0, CT c1)
	{
		return self(this->MoveTo(c0, c1));
	}

	/// RelativeMoveTo
	BriefPathNVSpec& m(CT c0, CT c1)
	{
		return self(this->RelativeMoveTo(c0, c1));
	}

	/// LineTo
	BriefPathNVSpec& L(CT c0, CT c1)
	{
		return self(this->LineTo(c0, c1));
	}

	/// RelativeLineTo
	BriefPathNVSpec& l(CT c0, CT c1)
	{
		return self(this->RelativeLineTo(c0, c1));
	}

	/// HorizontalLineTo
	BriefPathNVSpec& H(CT c0)
	{
		return self(this->HorizontalLineTo(c0));
	}

	/// RelativeHorizontalLineTo
	BriefPathNVSpec& h(CT c0)
	{
		return self(this->RelativeHorizontalLineTo(c0));
	}

	/// VerticalLineTo
	BriefPathNVSpec& V(CT c0)
	{
		return self(this->VerticalLineTo(c0));
	}

	/// RelativeVerticalLineTo
	BriefPathNVSpec& v(CT c0)
	{
		return self(this->RelativeVerticalLineTo(c0));
	}

	/// QuadraticCurveTo
	BriefPathNVSpec& Q(CT c0, CT c1, CT c2, CT c3)
	{
		return self(this->QuadraticCurveTo(c0, c1, c2, c3));
	}

	/// RelativeQuadraticCurveTo
	BriefPathNVSpec& q(CT c0, CT c1, CT c2, CT c3)
	{
		return self(this->RelativeQuadraticCurveTo(c0, c1, c2, c3));
	}

	/// CubicCurveTo
	BriefPathNVSpec& C(CT c0, CT c1, CT c2, CT c3, CT c4, CT c5)
	{
		return self(this->CubicCurveTo(c0, c1, c2, c3, c4, c5));
	}

	/// RelativeCubicCurveTo
	BriefPathNVSpec& c(CT c0, CT c1, CT c2, CT c3, CT c4, CT c5)
	{
		return self(this->RelativeCubicCurveTo(c0, c1, c2, c3, c4, c5));
	}

	/// SmoothQuadraticCurveTo
	BriefPathNVSpec& T(CT c0, CT c1)
	{
		return self(this->SmoothQuadraticCurveTo(c0, c1));
	}

	/// RelativeSmoothQuadraticCurveTo
	BriefPathNVSpec& t(CT c0, CT c1)
	{
		return self(this->RelativeSmoothQuadraticCurveTo(c0, c1));
	}

	/// SmoothCubicCurveTo
	BriefPathNVSpec& S(CT c0, CT c1, CT c2, CT c3)
	{
		return self(this->SmoothCubicCurveTo(c0, c1, c2, c3));
	}

	/// RelativeSmoothCubicCurveTo
	BriefPathNVSpec& s(CT c0, CT c1, CT c2, CT c3)
	{
		return self(this->RelativeSmoothCubicCurveTo(c0, c1, c2, c3));
	}

	/// ArcTo
	BriefPathNVSpec& A(CT c0, CT c1, CT c2, CT c3, CT c4, CT c5, CT c6)
	{
		return self(this->ArcTo(c0, c1, c2, c3, c4, c5, c6));
	}

	/// RelativeArcTo
	BriefPathNVSpec& a(CT c0, CT c1, CT c2, CT c3, CT c4, CT c5, CT c6)
	{
		return self(this->RelativeArcTo(c0, c1, c2, c3, c4, c5, c6));
	}
};

} // namespace oglplus

#endif // NV_path_rendering

#endif // include guard
