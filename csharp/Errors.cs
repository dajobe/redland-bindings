//
// Errors.cs: Redland Error Handling
//
// $Id$
//
// Author:
//	Edd Dumbill (edd@usefulinc.com)
//
// (C) 2004 Edd Dumbill
//

using System;
using System.Runtime.InteropServices;

namespace Redland {

	public enum LogLevel {
		None = 0,
		Debug,
		Info,
		Warn,
		Error,
		Fatal
	};

	public enum Facility {
		None = 0,
		Concepts,
		Digest,
		Files,
		Hash,
		Init,
		Iterator,
		List,
		Model,
		Node,
		Parser,
		Query,
		Serializer,
		Statement,
		Storage,
		Stream,
		Uri,
		Utf8,
	};

	public class Locator
	{
		int l, c, b;
		string file;
		string uri;

		public int Line {
			get {
				return l;
			}
		}
		
		public int Column {
			get {
				return c;
			}
		}
		
		public int Byte {
			get {
				return b;
			}
		}
		
		public string File {
			get {
				return file;
			}
		}
		
		public string Uri {
			get {
				return uri;
			}
		}

		[DllImport ("librdf")]
		static extern int raptor_locator_line (IntPtr locator);
		[DllImport ("librdf")]
		static extern int raptor_locator_column (IntPtr locator);
		[DllImport ("librdf")]
		static extern int raptor_locator_byte (IntPtr locator);
		[DllImport ("librdf")]
		static extern IntPtr raptor_locator_file (IntPtr locator);
		[DllImport ("librdf")]
		static extern IntPtr raptor_locator_uri (IntPtr locator);

		public Locator (IntPtr rloc)
		{
			l = raptor_locator_line (rloc);
			c = raptor_locator_column (rloc);
			b = raptor_locator_byte (rloc);
			IntPtr istr = raptor_locator_file (rloc);
			if (istr != IntPtr.Zero) {
				file = Marshal.PtrToStringAuto (istr);
			}
			istr = raptor_locator_uri (rloc);
			if (istr != IntPtr.Zero) {
				uri = Marshal.PtrToStringAuto (istr);
			}
		}
	}

	public class LogMessage 
	{
		protected Facility facility;
		protected LogLevel level;
		protected int code;
		protected Locator locator;
		protected string message;

		[DllImport ("librdf")]
		static extern int librdf_log_message_code (IntPtr message);
		[DllImport ("librdf")]
		static extern int librdf_log_message_level (IntPtr message);
		[DllImport ("librdf")]
		static extern int librdf_log_message_facility (IntPtr message);
		[DllImport ("librdf")]
		static extern IntPtr  librdf_log_message_message (IntPtr message);
		[DllImport ("librdf")]
		static extern IntPtr librdf_log_message_locator (IntPtr message);

		public LogMessage (IntPtr message)
		{
			// the incoming message is transient: we need 
			// to copy it, and not wrap it
			code = librdf_log_message_code (message);
			level = (LogLevel) librdf_log_message_level (message);
			facility = (Facility) librdf_log_message_facility (message);
			IntPtr istr = librdf_log_message_message (message);
			if (istr != IntPtr.Zero) {
				this.message = Marshal.PtrToStringAuto (istr);
			}
			IntPtr iloc = librdf_log_message_locator (message);
			if (iloc != IntPtr.Zero) {
				locator = new Locator (iloc);
			}
		}

		public override string ToString () 
		{
			// TODO: add some text string for the severity level
			if (Locator == null) {
				return message;
			} else {
				string ret = String.Format ("{0} at line {1}", 
						message, locator.Line);
				if (locator.Column >= 0)
					ret = ret + String.Format (", position {0}",
							locator.Column);
				return ret;
			}
		}

		public Facility Facility {
			get {
				return facility;
			}
		}

		public LogLevel Level {
			get {
				return level;
			}
		}

		public int Code {
			get {
				return code;
			}
		}

		public Locator Locator {
			get {
				return locator;
			}
		}

		public string Message {
			get {
				return message;
			}
		}
	}

	
	public abstract class RedlandFault : Exception
	{
		public LogMessage [] Messages;
		public RedlandFault (string msg, LogMessage [] messages) :
			base (msg)
		{
			// we have the string array to accumulate a list of
			// errors that can happen during an operation
			Messages = messages;
		}
	}

	public class RedlandError : RedlandFault
	{
		public RedlandError (string msg, LogMessage [] messages) :
			base (msg, messages)
		{
		}
	}
}

