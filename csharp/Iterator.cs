//
// Iterator.cs:
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

	public class Iterator : IWrapper, IEnumerator, IDisposable {
		
		IntPtr iterator = IntPtr.Zero;

		bool disposed = false;

		public IntPtr Handle {
			get { return iterator; }
		}

		[DllImport ("librdf")]
		static extern int librdf_iterator_end (IntPtr iterator);
		
		[DllImport ("librdf")]
		static extern IntPtr librdf_iterator_get_object (IntPtr iterator);

		[DllImport ("librdf")]
		static extern int librdf_iterator_next (IntPtr iterator);

		// IEnumerator implementation
		public object Current {
			get { 
				IntPtr raw_ret = librdf_iterator_get_object (iterator);
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
			// underlying librdf method returns non-0 when done
			// we want to return true while there's next to move to
			return (librdf_iterator_next (iterator) == 0);
		}

		public void Reset ()
		{
			throw new NotSupportedException ();
		}

		[DllImport ("librdf")]
		static extern int libdf_iterator_end (IntPtr iterator);

		public bool End {
			get {
				return (librdf_iterator_end (iterator) != 0);
			}
		}

		internal Iterator (IntPtr iterator)
		{
			this.iterator = iterator;
		}

		[DllImport ("librdf")]
		static extern void librdf_free_iterator (IntPtr iterator);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing is true, dispose of
				// managed resources

				if (iterator != IntPtr.Zero) {
					librdf_free_iterator (iterator);
					iterator = IntPtr.Zero;
				}
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
	}
}

