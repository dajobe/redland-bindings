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

namespace Rdf {

	public class Iterator : IWrapper, IEnumerator {
		
		IntPtr iterator;
		int pos = 0;

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
				Node node;
				IntPtr raw_ret =librdf_iterator_get_object (iterator);
				node = new Node (raw_ret);
				return node;						 
			}
		}

		public bool MoveNext ()
		{
			int r =	librdf_iterator_next (iterator);
			if (r != 0)
				return false;
			else
				return true;
		}

		public void Reset ()
		{
			throw new NotSupportedException ();
		}

		[DllImport ("librdf")]
		static extern int libdf_iterator_end (IntPtr iterator);

		public bool End {
			get {
				int r = librdf_iterator_end (iterator);
				if (r != 0)
					return true;
				else
					return false;
			}
		}

		internal Iterator (IntPtr iterator)
		{
			this.iterator = iterator;
		}
	}
}

