/**
 *  @file oglplus/transform_feedback.hpp
 *  @brief TransformFeedback object wrappers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TRANSFORM_FEEDBACK_1107121519_HPP
#define OGLPLUS_TRANSFORM_FEEDBACK_1107121519_HPP

#include <oglplus/config/compiler.hpp>
#include <oglplus/glfunc.hpp>
#include <oglplus/object/wrapper.hpp>
#include <oglplus/error/object.hpp>
#include <oglplus/transform_feedback_target.hpp>
#include <oglplus/transform_feedback_mode.hpp>
#include <oglplus/transform_feedback_type.hpp>
#include <cassert>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_transform_feedback2

/// Class wrapping transform feedback construction/destruction functions
/** @note Do not use this class directly, use TransformFeedback instead.
 *
 *  @glsymbols
 *  @glfunref{GenTransformFeedbacks}
 *  @glfunref{DeleteTransformFeedbacks}
 *  @glfunref{IsTransformFeedback}
 */
template <>
class ObjGenDelOps<tag::TransformFeedback>
{
protected:
	static void Gen(tag::Generate, GLsizei count, GLuint* names)
	{
		assert(names != nullptr);
		OGLPLUS_GLFUNC(GenTransformFeedbacks)(count, names);
		OGLPLUS_CHECK_SIMPLE(GenTransformFeedbacks);
	}
#if GL_VERSION_4_5 || GL_ARB_direct_state_access
	static void Gen(tag::Create, GLsizei count, GLuint* names)
	{
		assert(names != nullptr);
		OGLPLUS_GLFUNC(CreateTransformFeedbacks)(count, names);
		OGLPLUS_CHECK_SIMPLE(CreateTransformFeedbacks);
	}
#endif

	static void Delete(GLsizei count, GLuint* names)
	{
		assert(names != nullptr);
		OGLPLUS_GLFUNC(DeleteTransformFeedbacks)(count, names);
		OGLPLUS_VERIFY_SIMPLE(DeleteTransformFeedbacks);
	}

	static Boolean IsA(GLuint name)
	{
		Boolean result(
			OGLPLUS_GLFUNC(IsTransformFeedback)(name),
			std::nothrow
		);
		OGLPLUS_VERIFY_SIMPLE(IsTransformFeedback);
		return result;
	}
};

/// TransformFeedback binding operations
template <>
class ObjBindingOps<tag::TransformFeedback>
{
private:
	static GLenum _binding_query(TransformFeedbackTarget target);
protected:
	static GLuint _binding(TransformFeedbackTarget target);
public:
	/// TransformFeedback bind targets
	typedef TransformFeedbackTarget Target;

	/// Returns the current TransformFeedback bound to specified @p target
	/**
	 *  @glsymbols
	 *  @glfunref{GetIntegerv}
	 */
	static TransformFeedbackName Binding(Target target)
	{
		return TransformFeedbackName(_binding(target));
	}

	/// Binds the specified @p transform feedback to the specified @p target
	/**
	 *  @glsymbols
	 *  @glfunref{BindTransformFeedback}
	 */
	static void Bind(
		Target target,
		TransformFeedbackName tfb
	)
	{
		OGLPLUS_GLFUNC(BindTransformFeedback)(
			GLenum(target),
			GetGLName(tfb)
		);
		OGLPLUS_VERIFY(
			BindTransformFeedback,
			ObjectError,
			Object(tfb).
			BindTarget(target)
		);
	}
};

#endif // GL_VERSION_4_0

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0

/// Class lifetime of which controls the (de)activation of TFB
/** This class activates transform feedback mode when it is
 *  constructed and deactivates it in destructor. It is a more
 *  robust and preferred mode of transform feedback activation
 *  and deactivation.
 *
 *  This class is non-copyable.
 *
 *  @glsymbols
 *  @glfunref{BeginTransformFeedback}
 *  @glfunref{EndTransformFeedback}
 */
class TransformFeedbackActivator
{
private:
	bool _active;
public:
	/// Begins transform feedback
	/**
	 *  @glsymbols
	 *  @glfunref{BeginTransformFeedback}
	 */
	TransformFeedbackActivator(TransformFeedbackPrimitiveType mode)
	 : _active(true)
	{
		OGLPLUS_GLFUNC(BeginTransformFeedback)(GLenum(mode));
		OGLPLUS_VERIFY(
			BeginTransformFeedback,
			Error,
			EnumParam(mode)
		);
	}

#if !OGLPLUS_NO_DELETED_FUNCTIONS
	/// Copying is disabled
	TransformFeedbackActivator(const TransformFeedbackActivator&) = delete;
#else
private:
	TransformFeedbackActivator(const TransformFeedbackActivator&);
public:
#endif

	TransformFeedbackActivator(TransformFeedbackActivator&& tmp)
	 : _active(tmp._active)
	{
		tmp._active = false;
	}

	void Finish(void)
	{
		if(_active)
		{
			OGLPLUS_GLFUNC(EndTransformFeedback)();
			OGLPLUS_VERIFY_SIMPLE(EndTransformFeedback);
			_active = false;
		}
	}

	/// Ends transform feedback
	/**
	 *  @glsymbols
	 *  @glfunref{EndTransformFeedback}
	 */
	~TransformFeedbackActivator(void)
	{
		try { Finish(); }
		catch(...){ }
	}
};

/// Class lifetime of which controls the pausing/resuming of TFB
/** This class pauses active transform feedback when it is
 *  constructed and resumes it in destructor. It is a more
 *  robust and preferred mode of transform feedback activation
 *  and deactivation.
 *
 *  This class is non-copyable.
 *
 *  @glsymbols
 *  @glfunref{PauseTransformFeedback}
 *  @glfunref{ResumeTransformFeedback}
 */
class TransformFeedbackPauser
{
private:
	bool _paused;
public:
	/// Pauses transform feedback
	/**
	 *  @glsymbols
	 *  @glfunref{PauseTransformFeedback}
	 */
	TransformFeedbackPauser(void)
	 : _paused(true)
	{
		OGLPLUS_GLFUNC(PauseTransformFeedback)();
		OGLPLUS_VERIFY_SIMPLE(PauseTransformFeedback);
	}

#if !OGLPLUS_NO_DELETED_FUNCTIONS
	TransformFeedbackPauser(const TransformFeedbackPauser&) = delete;
#else
private:
	TransformFeedbackPauser(const TransformFeedbackPauser&);
public:
#endif

	TransformFeedbackPauser(TransformFeedbackPauser&& tmp)
	 : _paused(tmp._paused)
	{
		tmp._paused = false;
	}

	/// Explicitly resumes transform feedback
	/**
	 *  @glsymbols
	 *  @glfunref{ResumeTransformFeedback}
	 */
	void Resume(void)
	{
		if(_paused)
		{
			OGLPLUS_GLFUNC(ResumeTransformFeedback)();
			OGLPLUS_VERIFY_SIMPLE(ResumeTransformFeedback);
			_paused = false;
		}
	}

	/// Resumes transform feedback
	/**
	 *  @glsymbols
	 *  @glfunref{ResumeTransformFeedback}
	 */
	~TransformFeedbackPauser(void)
	{
		try{ Resume(); }
		catch(...){ }
	}
};

/// Common transform feedback operations
/** @note Do not use this class directly, use TransformFeedback
 *  or DefaultTransformFeedback instead.
 */
template <>
class ObjCommonOps<tag::TransformFeedback>
 : public TransformFeedbackName
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_transform_feedback2
 , public ObjBindingOps<tag::TransformFeedback>
#endif
{
protected:
	ObjCommonOps(TransformFeedbackName name)
	OGLPLUS_NOEXCEPT(true)
	 : TransformFeedbackName(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjCommonOps(ObjCommonOps&&) = default;
	ObjCommonOps(const ObjCommonOps&) = default;
	ObjCommonOps& operator = (ObjCommonOps&&) = default;
	ObjCommonOps& operator = (const ObjCommonOps&) = default;
#else
	typedef TransformFeedbackName _base1;
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_transform_feedback2
	typedef ObjBindingOps<tag::TransformFeedback> _base2;
#endif

	ObjCommonOps(ObjCommonOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	 : _base1(static_cast<_base1&&>(temp))
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_transform_feedback2
	 , _base2(static_cast<_base2&&>(temp))
#endif
	{ }

	ObjCommonOps(const ObjCommonOps& that)
	OGLPLUS_NOEXCEPT(true)
	 : _base1(static_cast<const _base1&>(that))
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_transform_feedback2
	 , _base2(static_cast<const _base2&>(that))
#endif
	{ }

	ObjCommonOps& operator = (ObjCommonOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	{
		_base1::operator = (static_cast<_base1&&>(temp));
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_transform_feedback2
		_base2::operator = (static_cast<_base2&&>(temp));
#endif
		return *this;
	}

	ObjCommonOps& operator = (const ObjCommonOps& that)
	OGLPLUS_NOEXCEPT(true)
	{
		_base1::operator = (static_cast<const _base1&>(that));
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_transform_feedback2
		_base2::operator = (static_cast<const _base2&>(that));
#endif
		return *this;
	}
#endif
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_transform_feedback2
	using ObjBindingOps<tag::TransformFeedback>::Bind;

	/// Binds this transform feedback to the specified @p target
	/**
	 *  @glsymbols
	 *  @glfunref{BindTransformFeedback}
	 */
	void Bind(Target target = Target::TransformFeedback) const
	{
		Bind(target, *this);
	}
#endif
	/// Begin the transform feedback mode
	/** Consider using an instance of Activator class for more robustness.
	 *  @throws Error
	 *
	 *  @see Activator
	 *  @see End
	 *
	 *  @glsymbols
	 *  @glfunref{BeginTransformFeedback}
	 */
	static void Begin(TransformFeedbackPrimitiveType mode)
	{
		OGLPLUS_GLFUNC(BeginTransformFeedback)(GLenum(mode));
		OGLPLUS_VERIFY(
			BeginTransformFeedback,
			Error,
			EnumParam(mode)
		);
	}

	/// Begin the transform feedback mode with POINTS
	static void BeginPoints(void)
	{
		Begin(TransformFeedbackPrimitiveType::Points);
	}

	/// Begin the transform feedback mode with LINES
	static void BeginLines(void)
	{
		Begin(TransformFeedbackPrimitiveType::Lines);
	}

	/// Begin the transform feedback mode with TRIANGLES
	static void BeginTriangles(void)
	{
		Begin(TransformFeedbackPrimitiveType::Triangles);
	}

	/// End the transform feedback mode
	/** Consider using an instance of Activator class for more robustness.
	 *  @throws Error
	 *
	 *  @see Activator
	 *  @see Begin
	 *
	 *  @glsymbols
	 *  @glfunref{EndTransformFeedback}
	 */
	static void End(void)
	{
		OGLPLUS_GLFUNC(EndTransformFeedback)();
		OGLPLUS_VERIFY_SIMPLE(EndTransformFeedback);
	}

	/// Pause the transform feedback mode
	/** Consider using an instance of Pauser class for more robustness.
	 *  @throws Error
	 *
	 *  @see Pauser
	 *  @see Resume
	 *
	 *  @glsymbols
	 *  @glfunref{PauseTransformFeedback}
	 */
	static void Pause(void)
	{
		OGLPLUS_GLFUNC(PauseTransformFeedback)();
		OGLPLUS_VERIFY_SIMPLE(PauseTransformFeedback);
	}

	/// Resume the transform feedback mode
	/** Consider using an instance of Pauser class for more robustness.
	 *  @throws Error
	 *
	 *  @see Pauser
	 *  @see Pause
	 *
	 *  @glsymbols
	 *  @glfunref{ResumeTransformFeedback}
	 */
	static void Resume(void)
	{
		OGLPLUS_GLFUNC(ResumeTransformFeedback)();
		OGLPLUS_VERIFY_SIMPLE(ResumeTransformFeedback);
	}

	typedef TransformFeedbackActivator Activator;
	typedef TransformFeedbackPauser Pauser;
};

/// Wrapper for default feedback operations
/**
 *  @ingroup oglplus_objects
 *
 *  @glverreq{3,0}
 */
typedef ObjectZero<ObjZeroOps<tag::ImplicitSel, tag::TransformFeedback>>
	DefaultTransformFeedback;

#endif // GL_VERSION_3_0

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_transform_feedback2

/// TransformFeedback operations with explicit selector
typedef ObjectOps<tag::ImplicitSel, tag::TransformFeedback>
	TransformFeedbackOps;

/// An @ref oglplus_object encapsulating the OpenGL transform feedback functionality
/**
 *  @ingroup oglplus_objects
 */
typedef Object<TransformFeedbackOps> TransformFeedback;

#endif // GL_VERSION_4_0 || transform feedback 2

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/transform_feedback.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
