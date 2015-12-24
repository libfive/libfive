/**
 *  @file oglplus/ext/KHR_debug.hpp
 *  @brief Wrapper for the KHR_debug extension
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_EXT_KHR_DEBUG_1308270710_HPP
#define OGLPLUS_EXT_KHR_DEBUG_1308270710_HPP

#include <oglplus/extension.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/config/compiler.hpp>
#include <oglplus/string/def.hpp>
#include <oglplus/string/ref.hpp>
#include <oglplus/glfunc.hpp>
#include <oglplus/ext/KHR_debug/severity.hpp>
#include <oglplus/ext/KHR_debug/source.hpp>
#include <oglplus/ext/KHR_debug/type.hpp>

#include <cassert>
#include <cstddef>
#include <stack>
#include <functional>
#include <memory>
#include <unordered_set>
#include <iostream>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_KHR_debug

/// Wrapper for the KHR_debug extension
/**
 *  @glsymbols
 *  @glextref{KHR,debug}
 *
 *  @ingroup gl_extensions
 */
class KHR_debug
{
public:
	OGLPLUS_EXTENSION_CLASS(KHR, debug)

	/// Enables/disables messages with specific parameters
	static void Control(
		DebugSource source,
		DebugType type,
		DebugSeverity severity,
		GLsizei count,
		const GLuint* ids,
		Boolean enable
	)
	{
		OGLPLUS_GLFUNC(DebugMessageControl)(
			GLenum(source),
			GLenum(type),
			GLenum(severity),
			count, ids,
			enable._get()
		);
		OGLPLUS_VERIFY_SIMPLE(DebugMessageControl);
	}

	/// Enables/disables messages with specific parameters
	static void Control(
		DebugSource source,
		DebugType type,
		DebugSeverity severity,
		bool enable
	)
	{
		Control(source, type, severity, 0, nullptr, enable);
	}

	/// Enables/disables messages with specific parameters
	template <std::size_t N>
	static void Control(
		DebugSource source,
		DebugType type,
		DebugSeverity severity,
		const GLuint (&ids)[N],
		bool enable
	)
	{
		Control(source, type, severity, N, ids, enable);
	}

	/// Structure containing data passed to Callback functor
	struct CallbackData
	{
		/// The source of the debug message
		DebugSource source;

		/// The type of the debug message
		DebugType type;

		/// The id of the debug message
		GLuint id;

		/// The severity of the debug message
		DebugSeverity severity;

		/// The length of th debug message
		GLsizei length;

		/// The debug message
		const GLchar* message;
	};

	/// Type of a callback functor processing debug output
	/** OGLplus implements several callbacks that can be used
	 *  with this extension:
	 *  @see KHR_debug_Unique
	 *  @see KHR_debug_Tree
	 *  @see KHR_debug_ToXML
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
				data.source = DebugSource(source);
				data.type = DebugType(type);
				data.id = id;
				data.severity = DebugSeverity(severity);
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
		LogSink(Callback callback)
		 : _callback(callback)
		 , _prev_callback(nullptr)
		 , _prev_context(nullptr)
		{
			// get the previous callback
			GLDEBUGPROC _tmp_callback = nullptr;
			void** _tmp_ptr=reinterpret_cast<void**>(&_tmp_callback);
			OGLPLUS_GLFUNC(GetPointerv)(
				GL_DEBUG_CALLBACK_FUNCTION,
				_tmp_ptr
			);
			OGLPLUS_IGNORE(GetPointerv);
			_prev_callback = _tmp_callback;

			//get the previous context
			OGLPLUS_GLFUNC(GetPointerv)(
				GL_DEBUG_CALLBACK_USER_PARAM,
				&_prev_context
			);
			OGLPLUS_IGNORE(GetPointerv);

			OGLPLUS_GLFUNC(DebugMessageCallback)(
				GLDEBUGPROC(&LogSink::_gl_debug_proc),
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
		~LogSink(void)
		{
			if(_prev_callback)
			{
				OGLPLUS_GLFUNC(DebugMessageCallback)(
					_prev_callback,
					_prev_context
				);
			}
		}
	};

	/// Inserts a new message into the debug output
	static void InsertMessage(
		DebugSource source,
		DebugType type,
		GLuint id,
		DebugSeverity severity,
		GLint length,
		const GLchar* buffer
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
	static void InsertMessage(
		DebugSource source,
		DebugType type,
		GLuint id,
		DebugSeverity severity,
		StrCRef message
	)
	{
		InsertMessage(
			source,
			type,
			id,
			severity,
			message.size(),
			message.c_str()
		);
	}

	/// Inserts a new message into the debug output
	template <std::size_t N>
	static void InsertMessage(
		DebugSource source,
		DebugType type,
		GLuint id,
		DebugSeverity severity,
		const GLchar (&message)[N]
	)
	{
		InsertMessage(
			source,
			type,
			id,
			severity,
			N,
			message
		);
	}

	/// Pushes a debug group
	static void PushGroup(
		DebugSource source,
		GLuint id,
		GLsizei length,
		const GLchar* message
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

	/// Pushes a debug group
	static void PushGroup(
		DebugSource source,
		GLuint id,
		StrCRef message
	)
	{
		PushGroup(source, id, message.size(), message.c_str());
	}

	/// Pushes a debug group
	template <std::size_t N>
	static void PushGroup(
		DebugSource source,
		GLuint id,
		const GLchar (&message)[N]
	)
	{
		PushGroup(source, id, N, message);
	}

	/// Pops a debug group
	static void PopGroup(void)
	{
		OGLPLUS_GLFUNC(PopDebugGroup)();
		OGLPLUS_VERIFY_SIMPLE(PopDebugGroup);
	}

	/// Enables or disables synchronous debug output
	static void Synchronous(bool enable = true)
	{
		if(enable)
		{
			OGLPLUS_GLFUNC(Enable)(GL_DEBUG_OUTPUT_SYNCHRONOUS);
			OGLPLUS_VERIFY_SIMPLE(Enable);
		}
		else
		{
			OGLPLUS_GLFUNC(Disable)(GL_DEBUG_OUTPUT_SYNCHRONOUS);
			OGLPLUS_VERIFY_SIMPLE(Disable);
		}
	}

	/// Enables or disables asynchronous debug output
	static void Asynchronous(bool enable = true)
	{
		Synchronous(!enable);
	}

};

template <typename Essence>
class KHR_debug_CallbackWithEssence
{
private:
	std::shared_ptr<Essence> essence;
public:
	typedef typename KHR_debug::Callback Callback;

	KHR_debug_CallbackWithEssence(
		typename Essence::CtrParam param
	): essence(std::make_shared<Essence>(param))
	{ }

	void operator()(const KHR_debug::CallbackData& data)
	{
		essence->Call(data);
	}

	operator Callback (void) const
	{
		return Callback(*this);
	}
};

class KHR_debug_UniqueEssence
{
private:
	typedef KHR_debug::Callback Callback;
	Callback _callback;

	String buffer;
	std::unordered_set<String> already_done;

	KHR_debug_UniqueEssence(const KHR_debug_UniqueEssence&);
public:
	typedef const Callback& CtrParam;

	KHR_debug_UniqueEssence(const Callback& callback)
	 : _callback(callback)
	{ }

	void Call(const KHR_debug::CallbackData& data);
};

#if OGLPLUS_DOCUMENTATION_ONLY
/// Filter for KHR_debug removing duplicate messages
/** An implementation of KHR_debug::Callback that removes duplicate
 *  messages, and passes them to another callback, i.e. every unique message
 *  from the debug is passed only once to another callback internally
 *  referenced by instances of this class.
 *
 *  @ingroup gl_extensions
 */
class KHR_debug_Unique
{
public:
	/// Construction takes another callback implementation
	KHR_debug_Unique(KHR_debug::Callback);

	/// Conversion to Callback type for the KHR_debug ext wrapper
	operator KHR_debug::Callback (void) const;
};
#else
typedef KHR_debug_CallbackWithEssence<KHR_debug_UniqueEssence>
	KHR_debug_Unique;
#endif


class KHR_debug_TreeEssence
{
private:
	std::ostream& dbgout;

	KHR_debug_TreeEssence(const KHR_debug_TreeEssence&);
public:
	typedef std::ostream& CtrParam;

	KHR_debug_TreeEssence(std::ostream& out);
	~KHR_debug_TreeEssence(void);

	void Call(const KHR_debug::CallbackData& data);
};

#if OGLPLUS_DOCUMENTATION_ONLY
/// Filter for KHR_debug printing a simple tree to a standard output.
/** An implementation of KHR_debug::Callback that prints the debug
 *  messages formatted as a simple tree, where the individual messages and
 *  their properties are represented as branches.
 *
 *  @ingroup gl_extensions
 */
class KHR_debug_Tree
{
public:
	/// Constructor takes a reference to standard output stream
	KHR_debug_Tree(std::ostream&);

	/// Conversion to Callback type for the KHR_debug ext wrapper
	operator KHR_debug::Callback (void) const;
};
#else
typedef KHR_debug_CallbackWithEssence<KHR_debug_TreeEssence>
	KHR_debug_Tree;
#endif


class KHR_debug_ToXMLEssence
{
private:
	std::ostream& dbgout;

	KHR_debug_ToXMLEssence(const KHR_debug_ToXMLEssence&);
public:
	typedef std::ostream& CtrParam;

	KHR_debug_ToXMLEssence(std::ostream& out);
	~KHR_debug_ToXMLEssence(void);

	void Call(const KHR_debug::CallbackData& data);
};

#if OGLPLUS_DOCUMENTATION_ONLY
/// Filter for KHR_debug formatting the debug output into XML
/** An implementation of KHR_debug::Callback that prints the debug
 *  messages formatted as an XML document.
 *
 *  @ingroup gl_extensions
 */
class KHR_debug_ToXML
{
public:
	/// Constructor takes a reference to standard output stream
	KHR_debug_ToXML(std::ostream&);

	/// Conversion to Callback type for the KHR_debug ext wrapper
	operator KHR_debug::Callback (void) const;
};
#else
typedef KHR_debug_CallbackWithEssence<KHR_debug_ToXMLEssence>
	KHR_debug_ToXML;
#endif

#endif // KHR_debug

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/ext/KHR_debug.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
