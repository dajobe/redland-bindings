//
// Stream.cs: Redland Statement Stream class. A class encapsulating 
//	      a sequence of Statements, such as those returned from a Model
//	      query. You should not normally find yourself needing to use this
//	      class explicitly.
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

	public class Stream : IWrapper, IEnumerator {

		IntPtr stream;

		public IntPtr Handle {
			get { return stream; }
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_stream_get_object (IntPtr stream);

		public object Current {
			get {
				Statement stm;
				IntPtr raw_ret = librdf_stream_get_object (stream);
				stm = new Statement (raw_ret);
				return stm;
			}
		}

		[DllImport ("librdf")]
		static extern int librdf_stream_next (IntPtr stream);

		public bool MoveNext ()
		{
			int r = librdf_stream_next (stream);
			if (r != 0)
				return false;
			else
				return true;
		}

		public void Reset ()
		{
			throw new NotSupportedException ("The invoked method is not supported");
		}

		[DllImport ("librdf")]
		static extern int librdf_stream_end (IntPtr stream);

		public bool End {
			get {
				int r = librdf_stream_end (stream);
				if (r != 0)
					return true;
				else
					return false;
			}
		}

		internal Stream (IntPtr raw)
		{
			stream = raw;
		}

		[DllImport ("librdf")]
		static extern void librdf_free_stream (IntPtr stream);

		~Stream ()
		{
			librdf_free_stream (stream);
		}


	}
}
