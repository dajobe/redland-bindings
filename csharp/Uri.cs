//
// Uri.cs: Redland Uri class.
//
// $Id$
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren
//

using System;
using System.Runtime.InteropServices;

namespace Redland {

	public class Uri : IWrapper, IDisposable {

		IntPtr uri = IntPtr.Zero;

		bool disposed = false;

		public IntPtr Handle {
			get { return uri; }
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_uri (IntPtr world, IntPtr uri_str);

		private Uri (World world, string uri_str)
		{
			IntPtr iuri_str = Marshal.StringToHGlobalAuto (uri_str);
			uri = librdf_new_uri (world.Handle, iuri_str);
			Marshal.FreeHGlobal (iuri_str);
			// Console.WriteLine ("Making URI from string {0} giving handle {1}", uri_str, uri);
		}

		public Uri (string uri)
			: this (Redland.World, uri)
		{
		}

		[DllImport ("librdf")]
		static extern void librdf_free_uri (IntPtr uri);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing is true then dispose
				// of managed resources

				if (uri != IntPtr.Zero) {
					librdf_free_uri (uri);
					uri = IntPtr.Zero;
				}
				disposed = true;
			}
		}

		public void Dispose ()
		{
			Dispose (true);
			GC.SuppressFinalize (this);
		}

		~Uri ()
		{
			Dispose (false);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_uri_to_string (IntPtr uri);

		public override string ToString ()
		{
			IntPtr istr = librdf_uri_to_string (uri);
			return Marshal.PtrToStringAuto (istr);
		}
	}
}
