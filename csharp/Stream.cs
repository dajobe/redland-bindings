//
// Stream.cs: Redland Statement Stream class.
//
// $Id$
//
// A class encapsulating a sequence of Statements, such as those
// returned from a Model query. You should not normally find yourself
// needing to use this class explicitly.
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren
//

using System;
using System.Collections;
using System.Runtime.InteropServices;

namespace Redland {

	public class Stream : IWrapper, IEnumerator, IEnumerable, IDisposable {

		IntPtr stream = IntPtr.Zero;

		bool disposed = false;
		bool started = false;

		public IntPtr Handle {
			get { return stream; }
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_stream_get_object (IntPtr stream);

		public object Current {
			get {
				IntPtr raw_ret = librdf_stream_get_object (stream);
				// FIXME: throw exception if raw_ret is zero
				return new Statement (raw_ret);
			}
		}

		public Statement CurrentStatement {
			get {
				return (Statement) Current;
			}
		}

		[DllImport ("librdf")]
		static extern int librdf_stream_next (IntPtr stream);

		public bool MoveNext ()
		{
			if (started) {
				return (librdf_stream_next (stream) == 0);
			} else {
				started = true;
				IntPtr cur = librdf_stream_get_object (stream);
				return (cur != IntPtr.Zero);
			}
		}

		public void Reset ()
		{
			throw new NotSupportedException ("The invoked method is not supported");
		}

		[DllImport ("librdf")]
		static extern int librdf_stream_end (IntPtr stream);

		public bool End {
			get {
				return (librdf_stream_end (stream) != 0);
			}
		}

		internal Stream (IntPtr raw)
		{
			stream = raw;
		}

		[DllImport ("librdf")]
		static extern void librdf_free_stream (IntPtr stream);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing true, dispose managed resources

				if (stream != IntPtr.Zero) {
					librdf_free_stream (stream);
					stream = IntPtr.Zero;
				}
				disposed = true;
			}
		}

		public void Dispose ()
		{
			Dispose (true);
			GC.SuppressFinalize (this);
		}

		~Stream ()
		{
			Dispose (false);
		}

		public IEnumerator GetEnumerator () 
		{
			return this;
		}
	}
}
