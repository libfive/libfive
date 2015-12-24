/**
 *  @file oglplus/opt/debug_output.hpp
 *  @brief Wrapper for the GL debug output functionality
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OPT_DEBUG_OUTPUT_1209031534_HPP
#define OGLPLUS_OPT_DEBUG_OUTPUT_1209031534_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/string/ref.hpp>
#include <oglplus/object/wrapper.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/enumerations.hpp>
#include <oglplus/enums/debug_output_severity.hpp>
#include <oglplus/enums/debug_output_source.hpp>
#include <oglplus/enums/debug_output_type.hpp>

#include <cassert>
#include <stack>
#include <functional>
#include <memory>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3

/// Wrapper for the GL debug output functionality
/**
 */
class Debug
{
public:
	/// Debug output severity
	typedef DebugOutputSeverity Severity;

	/// Debug output source
	typedef DebugOutputSource Source;

	/// Debug output type
	typedef DebugOutputType Type;


	/// Enables/disables messages with specific parameters
	/**
	 *  @glsymbols
	 *  @glfunref{DebugMessageControl}
	 */
	static void MessageControl(
		DebugOutputSource source,
		DebugOutputType type,
		DebugOutputSeverity severity,
		Boolean enable
	)
	{
		OGLPLUS_GLFUNC(DebugMessageControl)(
			GLenum(source),
			GLenum(type),
			GLenum(severity),
			0, nullptr,
			enable._get()
		);
		OGLPLUS_VERIFY_SIMPLE(DebugMessageControl);
	}

	/// Structure containing data passed to Callback functor
	struct CallbackData
	{
		DebugOutputSource source;
		DebugOutputType type;
		GLuint id;
		DebugOutputSeverity severity;
		GLsizei length;
		const GLchar* message;
	};

	/// Type of a callback functor processing debug output
	typedef std::function<void (const CallbackData&)> Callback;

	/// Installs a custom callback processing the debug output
	/**
	 *  Instances of this class install a new Callback function
	 *  processing the debug output messages in the constructor
	 *  and restoring the previous callback when destroyed.
	 *
	 *  @glsymbols
	 *  @glfunref{DebugMessageCallback}
	 *  @glfunref{GetPointerv}
	 *  @gldefref{DEBUG_CALLBACK_FUNCTION}
	 *  @gldefref{DEBUG_CALLBACK_USER_PARAM}
	 */
	class LogSink
	{
	private:
		static void GLAPIENTRY _gl_debug_proc(
			GLenum source,
			GLenum type,
			GLuint id,
			GLenum severity,
			GLsizei length,
			const GLchar* message,
			const GLvoid* user_param
		)
		{
			LogSink* self = (LogSink*)(user_param);
			assert(self);
			if(self->_callback)
			{
				CallbackData data;
				data.source = DebugOutputSource(source);
				data.type = DebugOutputType(type);
				data.id = id;
				data.severity = DebugOutputSeverity(severity);
				data.length = length;
				data.message = message;
				self->_callback(data);
			}
		}

		Callback _callback;
		GLDEBUGPROC _prev_callback;
		void* _prev_context;
	public:
		/// Installs the @p callback and remembers the previous
		/**
		 *  @glsymbols
		 *  @glfunref{DebugMessageCallback}
		 */
		LogSink(Callback callback)
		 : _callback(callback)
		 , _prev_callback(nullptr)
		 , _prev_context(nullptr)
		{
			// get the previous callback
			GLDEBUGPROC _tmp_callback = nullptr;
			void** _tmp_ptr =
				reinterpret_cast<void**>(&_tmp_callback);
			OGLPLUS_GLFUNC(GetPointerv)(
				GL_DEBUG_CALLBACK_FUNCTION,
				_tmp_ptr
			);
			OGLPLUS_VERIFY_SIMPLE(GetPointerv);
			_prev_callback = _tmp_callback;

			//get the previous context
			OGLPLUS_GLFUNC(GetPointerv)(
				GL_DEBUG_CALLBACK_USER_PARAM,
				&_prev_context
			);
			OGLPLUS_VERIFY_SIMPLE(GetPointerv);

			OGLPLUS_GLFUNC(DebugMessageCallback)(
				&LogSink::_gl_debug_proc,
				static_cast<void*>(this)
			);
			OGLPLUS_VERIFY_SIMPLE(DebugMessageCallback);
		}

#if !OGLPLUS_NO_DELETED_FUNCTIONS
		/// LogSinks are not copyable
		LogSink(const LogSink&) = delete;
#else
	private:
		LogSink(const LogSink&);
	public:
#endif

		/// Restores the previous callback and its context
		/**
		 *  @glsymbols
		 *  @glfunref{DebugMessageCallback}
		 */
		~LogSink(void)
		{
			OGLPLUS_GLFUNC(DebugMessageCallback)(
				_prev_callback,
				_prev_context
			);
			OGLPLUS_VERIFY_SIMPLE(DebugMessageCallback);
		}
	};

	/// Pushes a group when constructed, pops in when destroyed
	class Group
	{
	public:
		/// Pushes a debug group with the specified parameters
		/**
		 *  @glsymbols
		 *  @glfunref{PushDebugGroup}
		 */
		Group(
			DebugOutputSource source,
			GLuint id,
			const GLchar* message,
			GLint length = -1
		)
		{
			OGLPLUS_GLFUNC(PushDebugGroup)(
				GLenum(source),
				id,
				length,
				message
			);
			OGLPLUS_VERIFY_SIMPLE(PushDebugGroup);
		}

		/// Pushes a debug group with the specified parameters
		/**
		 *  @overload
		 *
		 *  @glsymbols
		 *  @glfunref{PushDebugGroup}
		 */
		Group(
			DebugOutputSource source,
			GLuint id,
			StrCRef message
		)
		{
			OGLPLUS_GLFUNC(PushDebugGroup)(
				GLenum(source),
				id,
				message.size(),
				message.begin()
			);
			OGLPLUS_VERIFY_SIMPLE(PushDebugGroup);
		}

		/// Pushes a debug group with the specified parameters
		/**
		 *  @overload
		 *
		 *  @glsymbols
		 *  @glfunref{PushDebugGroup}
		 */
		Group(
			DebugOutputSource source,
			GLuint id,
			const String& message
		)
		{
			OGLPLUS_GLFUNC(PushDebugGroup)(
				GLenum(source),
				id,
				message.size(),
				message.c_str()
			);
			OGLPLUS_VERIFY_SIMPLE(PushDebugGroup);
		}

#if !OGLPLUS_NO_DELETED_FUNCTIONS
		/// Groups are not copyable
		Group(const Group&) = delete;
#else
	private:
		Group(const Group&);
	public:
#endif
		/// Pops a debug group with the specified parameters
		/**
		 *  @glsymbols
		 *  @glfunref{PopDebugGroup}
		 */
		~Group(void)
		{
			OGLPLUS_GLFUNC(PopDebugGroup)();
			OGLPLUS_VERIFY_SIMPLE(PopDebugGroup);
		}
	};

	/// Annotate @p object with the @p label with the specified @p length
	/**
	 *  @glsymbols
	 *  @glfunref{ObjectLabel}
	 */
	template <typename ObjTag>
	static void ObjectLabel(
		const ObjectName<ObjTag>& object,
		StrCRef label
	)
	{
		OGLPLUS_GLFUNC(ObjectLabel)(
			GetGLName(object),
			ObjTypeOps<ObjTag>::ObjectType(),
			label.size(),
			label.c_str()
		);
		OGLPLUS_VERIFY_SIMPLE(ObjectLabel);
	}

	/// Enables or disables synchronous debug output
	/**
	 *  @glsymbols
	 *  @glfunref{Enable}
	 *  @glfunref{Disable}
	 *  @gldefref{DEBUG_OUTPUT_SYNCHRONOUS}
	 */
	static void Synchronous(bool enable)
	{
		if(enable)
		{
			OGLPLUS_GLFUNC(Enable)(
				GL_DEBUG_OUTPUT_SYNCHRONOUS
			);
			OGLPLUS_VERIFY_SIMPLE(Enable);
		}
		else
		{
			OGLPLUS_GLFUNC(Disable)(
				GL_DEBUG_OUTPUT_SYNCHRONOUS
			);
			OGLPLUS_VERIFY_SIMPLE(Disable);
		}
	}

	/// Inserts a new message into the debug output
	/**
	 *  @glsymbols
	 *  @glfunref{DebugMessageInsert}
	 */
	static void InsertMessage(
		DebugOutputSource source,
		DebugOutputType type,
		GLuint id,
		DebugOutputSeverity severity,
		const GLchar* buffer,
		GLint length = -1
	)
	{
		OGLPLUS_GLFUNC(DebugMessageInsert)(
			GLenum(source),
			GLenum(type),
			id,
			GLenum(severity),
			length,
			buffer
		);
		OGLPLUS_VERIFY_SIMPLE(DebugMessageInsert);
	}

	/// Inserts a new message into the debug output
	/**
	 *  @overload
	 *
	 *  @glsymbols
	 *  @glfunref{DebugMessageInsert}
	 */
	static void InsertMessage(
		DebugOutputSource source,
		DebugOutputType type,
		GLuint id,
		DebugOutputSeverity severity,
		StrCRef message
	)
	{
		OGLPLUS_GLFUNC(DebugMessageInsert)(
			GLenum(source),
			GLenum(type),
			id,
			GLenum(severity),
			message.size(),
			message.c_str()
		);
		OGLPLUS_VERIFY_SIMPLE(DebugMessageInsert);
	}

	/// Inserts a new message into the debug output
	/**
	 *  @overload
	 *
	 *  @glsymbols
	 *  @glfunref{DebugMessageInsert}
	 */
	static void InsertMessage(
		DebugOutputSource source,
		DebugOutputType type,
		GLuint id,
		DebugOutputSeverity severity,
		const String& message
	)
	{
		OGLPLUS_GLFUNC(DebugMessageInsert)(
			GLenum(source),
			GLenum(type),
			id,
			GLenum(severity),
			message.size(),
			message.c_str()
		);
		OGLPLUS_VERIFY_SIMPLE(DebugMessageInsert);
	}
};
#endif

} // namespace oglplus

#endif // include guard
