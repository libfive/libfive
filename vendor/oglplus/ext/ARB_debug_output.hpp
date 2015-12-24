/**
 *  @file oglplus/ext/ARB_debug_output.hpp
 *  @brief Wrapper for the ARB_debug_output extension
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_EXT_ARB_DEBUG_OUTPUT_1203031902_HPP
#define OGLPLUS_EXT_ARB_DEBUG_OUTPUT_1203031902_HPP

#include <oglplus/extension.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/config/compiler.hpp>
#include <oglplus/string/ref.hpp>
#include <oglplus/string/def.hpp>
#include <oglplus/glfunc.hpp>
#include <oglplus/ext/ARB_debug_output/severity.hpp>
#include <oglplus/ext/ARB_debug_output/source.hpp>
#include <oglplus/ext/ARB_debug_output/type.hpp>

#include <cassert>
#include <stack>
#include <functional>
#include <memory>
#include <unordered_set>
#include <iostream>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_ARB_debug_output

/// Wrapper for the ARB_debug_output extension
/**
 *  @glsymbols
 *  @glextref{ARB,debug_output}
 *
 *  @ingroup gl_extensions
 */
class ARB_debug_output
{
public:
	OGLPLUS_EXTENSION_CLASS(ARB, debug_output)

	/// Enables/disables messages with specific parameters
	static void Control(
		DebugOutputARBSource source,
		DebugOutputARBType type,
		DebugOutputARBSeverity severity,
		Boolean enable
	)
	{
		OGLPLUS_GLFUNC(DebugMessageControlARB)(
			GLenum(source),
			GLenum(type),
			GLenum(severity),
			0, nullptr,
			enable._get()
		);
		OGLPLUS_VERIFY_SIMPLE(DebugMessageControlARB);
	}

	/// Structure containing data passed to Callback functor
	struct CallbackData
	{
		/// The source of the debug message
		DebugOutputARBSource source;

		/// The type of the debug message
		DebugOutputARBType type;

		/// The id of the debug message
		GLuint id;

		/// The severity of the debug message
		DebugOutputARBSeverity severity;

		/// The length of th debug message
		GLsizei length;

		/// The debug message
		const GLchar* message;
	};

	/// Type of a callback functor processing debug output
	/** OGLplus implements several callbacks that can be used
	 *  with this extension:
	 *  @see ARB_debug_output_Unique
	 *  @see ARB_debug_output_Tree
	 *  @see ARB_debug_output_ToXML
	 */
	typedef std::function<void (const CallbackData&)> Callback;

	/// Installs a custom callback processing the debug output
	/**
	 *  Instances of this class install a new Callback function
	 *  processing the debug output messages in the constructor
	 *  and restoring the previous callback when destroyed.
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
			GLvoid* user_param
		)
		{
			LogSink* self = static_cast<LogSink*>(user_param);
			assert(self);
			if(self->_callback)
			{
				CallbackData data;
				data.source = DebugOutputARBSource(source);
				data.type = DebugOutputARBType(type);
				data.id = id;
				data.severity = DebugOutputARBSeverity(severity);
				data.length = length;
				data.message = message;
				self->_callback(data);
			}
		}

		Callback _callback;
		GLDEBUGPROCARB _prev_callback;
		void* _prev_context;
	public:
		/// Installs the @p callback and remembers the previous
		LogSink(Callback callback)
		 : _callback(callback)
		 , _prev_callback(nullptr)
		 , _prev_context(nullptr)
		{
			// get the previous callback
			GLDEBUGPROCARB _tmp_callback = nullptr;
			void** _tmp_ptr=reinterpret_cast<void**>(&_tmp_callback);
			OGLPLUS_GLFUNC(GetPointerv)(
				GL_DEBUG_CALLBACK_FUNCTION_ARB,
				_tmp_ptr
			);
			OGLPLUS_IGNORE(GetPointerv);
			_prev_callback = _tmp_callback;

			//get the previous context
			OGLPLUS_GLFUNC(GetPointerv)(
				GL_DEBUG_CALLBACK_USER_PARAM_ARB,
				&_prev_context
			);
			OGLPLUS_IGNORE(GetPointerv);

			OGLPLUS_GLFUNC(DebugMessageCallbackARB)(
				GLDEBUGPROCARB(&LogSink::_gl_debug_proc),
				static_cast<void*>(this)
			);
			OGLPLUS_VERIFY_SIMPLE(DebugMessageCallbackARB);
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
		~LogSink(void)
		{
			if(_prev_callback)
			{
				OGLPLUS_GLFUNC(DebugMessageCallbackARB)(
					_prev_callback,
					_prev_context
				);
			}
		}
	};

	/// Enables or disables synchronous debug output
	static void Synchronous(bool enable)
	{
		if(enable)
		{
			OGLPLUS_GLFUNC(Enable)(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB);
			OGLPLUS_VERIFY_SIMPLE(Enable);
		}
		else
		{
			OGLPLUS_GLFUNC(Disable)(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB);
			OGLPLUS_VERIFY_SIMPLE(Disable);
		}
	}

	/// Inserts a new message into the debug output
	static void InsertMessage(
		DebugOutputARBSource source,
		DebugOutputARBType type,
		GLuint id,
		DebugOutputARBSeverity severity,
		const GLchar* buffer,
		GLint length = -1
	)
	{
		OGLPLUS_GLFUNC(DebugMessageInsertARB)(
			GLenum(source),
			GLenum(type),
			id,
			GLenum(severity),
			length,
			buffer
		);
		OGLPLUS_VERIFY_SIMPLE(DebugMessageInsertARB);
	}

	/// Inserts a new message into the debug output
	static void InsertMessage(
		DebugOutputARBSource source,
		DebugOutputARBType type,
		GLuint id,
		DebugOutputARBSeverity severity,
		StrCRef message
	)
	{
		OGLPLUS_GLFUNC(DebugMessageInsertARB)(
			GLenum(source),
			GLenum(type),
			id,
			GLenum(severity),
			GLsizei(message.size()),
			message.c_str()
		);
		OGLPLUS_VERIFY_SIMPLE(DebugMessageInsertARB);
	}
};

template <typename Essence>
class ARB_debug_output_CallbackWithEssence
{
private:
	std::shared_ptr<Essence> essence;
public:
	typedef typename ARB_debug_output::Callback Callback;

	ARB_debug_output_CallbackWithEssence(
		typename Essence::CtrParam param
	): essence(std::make_shared<Essence>(param))
	{ }

	void operator()(const ARB_debug_output::CallbackData& data)
	{
		essence->Call(data);
	}

	operator Callback (void) const
	{
		return Callback(*this);
	}
};

class ARB_debug_output_UniqueEssence
{
private:
	typedef ARB_debug_output::Callback Callback;
	Callback _callback;

	String buffer;
	std::unordered_set<String> already_done;

	ARB_debug_output_UniqueEssence(const ARB_debug_output_UniqueEssence&);
public:
	typedef const Callback& CtrParam;

	ARB_debug_output_UniqueEssence(const Callback& callback)
	 : _callback(callback)
	{ }

	void Call(const ARB_debug_output::CallbackData& data);
};

#if OGLPLUS_DOCUMENTATION_ONLY
/// Filter for ARB_debug_output removing duplicate messages
/** An implementation of ARB_debug_output::Callback that removes duplicate
 *  messages, and passes them to another callback, i.e. every unique message
 *  from the debug_output is passed only once to another callback internally
 *  referenced by instances of this class.
 *
 *  @ingroup gl_extensions
 */
class ARB_debug_output_Unique
{
public:
	/// Construction takes another callback implementation
	ARB_debug_output_Unique(ARB_debug_output::Callback);

	/// Conversion to Callback type for the ARB_debug_output ext wrapper
	operator ARB_debug_output::Callback (void) const;
};
#else
typedef ARB_debug_output_CallbackWithEssence<ARB_debug_output_UniqueEssence>
	ARB_debug_output_Unique;
#endif


class ARB_debug_output_TreeEssence
{
private:
	std::ostream& dbgout;

	ARB_debug_output_TreeEssence(const ARB_debug_output_TreeEssence&);
public:
	typedef std::ostream& CtrParam;

	ARB_debug_output_TreeEssence(std::ostream& out);
	~ARB_debug_output_TreeEssence(void);

	void Call(const ARB_debug_output::CallbackData& data);
};

#if OGLPLUS_DOCUMENTATION_ONLY
/// Filter for ARB_debug_output printing a simple tree to a standard output.
/** An implementation of ARB_debug_output::Callback that prints the debug
 *  messages formatted as a simple tree, where the individual messages and
 *  their properties are represented as branches.
 *
 *  @ingroup gl_extensions
 */
class ARB_debug_output_Tree
{
public:
	/// Constructor takes a reference to standard output stream
	ARB_debug_output_Tree(std::ostream&);

	/// Conversion to Callback type for the ARB_debug_output ext wrapper
	operator ARB_debug_output::Callback (void) const;
};
#else
typedef ARB_debug_output_CallbackWithEssence<ARB_debug_output_TreeEssence>
	ARB_debug_output_Tree;
#endif


class ARB_debug_output_ToXMLEssence
{
private:
	std::ostream& dbgout;

	ARB_debug_output_ToXMLEssence(const ARB_debug_output_ToXMLEssence&);
public:
	typedef std::ostream& CtrParam;

	ARB_debug_output_ToXMLEssence(std::ostream& out);
	~ARB_debug_output_ToXMLEssence(void);

	void Call(const ARB_debug_output::CallbackData& data);
};

#if OGLPLUS_DOCUMENTATION_ONLY
/// Filter for ARB_debug_output formatting the debug output into XML
/** An implementation of ARB_debug_output::Callback that prints the debug
 *  messages formatted as an XML document.
 *
 *  @ingroup gl_extensions
 */
class ARB_debug_output_ToXML
{
public:
	/// Constructor takes a reference to standard output stream
	ARB_debug_output_ToXML(std::ostream&);

	/// Conversion to Callback type for the ARB_debug_output ext wrapper
	operator ARB_debug_output::Callback (void) const;
};
#else
typedef ARB_debug_output_CallbackWithEssence<ARB_debug_output_ToXMLEssence>
	ARB_debug_output_ToXML;
#endif

#endif // ARB_debug_output

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/ext/ARB_debug_output.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
