//
// Iterator.cs: Redland Iterator class
//
// $Id$
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

	public class Iterator : IWrapper, IEnumerator, IEnumerable, IDisposable {
		
		private HandleRef handle;

		private bool disposed = false;
		private bool started = false;
		private World world = Redland.World.AddReference ();

		public HandleRef Handle {
			get { return handle; }
		}

		[DllImport ("librdf")]
		static extern int librdf_iterator_end (HandleRef iterator);
		
		[DllImport ("librdf")]
		static extern IntPtr librdf_iterator_get_object (HandleRef iterator);

		[DllImport ("librdf")]
		static extern int librdf_iterator_next (HandleRef iterator);

		// IEnumerator implementation
		public object Current {
			get { 
				IntPtr raw_ret = librdf_iterator_get_object (handle);
				// FIXME: throw exception if zero?
				return new Node (raw_ret);
			}
		}

		public Node CurrentNode {
			get {
				return (Node) Current;
			}
		}

		public bool MoveNext ()
		{
			if (started) {
				return (librdf_iterator_next (handle) == 0);
			} else {
				started = true;
				IntPtr cur = librdf_iterator_get_object (handle);
				return (cur != IntPtr.Zero);
			}
		}

		public void Reset ()
		{
			throw new NotSupportedException ();
		}

		internal Iterator (IntPtr iterator)
		{
			handle = new HandleRef (this, iterator);
		}

		[DllImport ("librdf")]
		static extern void librdf_free_iterator (HandleRef iterator);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing is true, dispose of
				// managed resources

				if (handle.Handle != IntPtr.Zero) {
					librdf_free_iterator (handle);
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

		~Iterator ()
		{
			Dispose (false);
		}

		public IEnumerator GetEnumerator () 
		{
			return this;
		}
	}
}

