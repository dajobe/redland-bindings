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

		private HandleRef handle;

		private bool disposed = false;
		private bool started = false;
		private World world = Redland.World.AddReference ();

		public HandleRef Handle {
			get { return handle; }
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_stream_get_object (HandleRef stream);

		public object Current {
			get {
				IntPtr raw_ret = librdf_stream_get_object (handle);
				// FIXME: throw exception if raw_ret is zero
				if(raw_ret != IntPtr.Zero)
					return new Statement (raw_ret);
	                        else 
        	                  	return null;
			}
		}

		public Statement CurrentStatement {
			get {
				return (Statement) Current;
			}
		}

		[DllImport ("librdf")]
		static extern int librdf_stream_next (HandleRef stream);

		public bool MoveNext ()
		{
			if (started) {
				return (librdf_stream_next (handle) == 0);
			} else {
				started = true;
				IntPtr cur = librdf_stream_get_object (handle);
				return (cur != IntPtr.Zero);
			}
		}

		public void Reset ()
		{
			throw new NotSupportedException ("The invoked method is not supported");
		}

		internal Stream (IntPtr raw)
		{
			handle = new HandleRef (this, raw);
		}

		[DllImport ("librdf")]
		static extern void librdf_free_stream (HandleRef stream);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing true, dispose managed resources

				if (handle.Handle != IntPtr.Zero) {
					librdf_free_stream (handle);
					handle = new HandleRef (this, IntPtr.Zero);
				}
				world.RemoveReference ();
				world = null;
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
