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

		private HandleRef handle;

		private bool disposed = false;
		private World world = Redland.World.AddReference ();

		public HandleRef Handle {
			get { return handle; }
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_uri (HandleRef world, IntPtr uri_str);

		public Uri (string uri_str)
		{
			IntPtr iuri_str = Util.StringToHGlobalUTF8 (uri_str);
			IntPtr uri = librdf_new_uri (world.Handle, iuri_str);
			handle = new HandleRef (this, uri);
			Marshal.FreeHGlobal (iuri_str);
			// Console.WriteLine ("Making URI from string {0} giving handle {1}", uri_str, uri);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_uri_from_uri (IntPtr uri);
	
		internal Uri (IntPtr iuri)
		{
			IntPtr uri = librdf_new_uri_from_uri (iuri);
			handle = new HandleRef (this, uri);
		}

		[DllImport ("librdf")]
		static extern int librdf_uri_equals (HandleRef first_uri, HandleRef second_uri);
		
		public override bool Equals (object o)
		{
			if (o == null)
 				return false;

			int i = librdf_uri_equals (handle, ((Uri) o).Handle);
			if (i == 0)
				return false;
			else
				return true;
		}

		public static bool operator == (Uri u1, Uri u2)
		{

			if (Object.Equals (u1, null))
				if (Object.Equals (u2, null))
					return true;
				else
					return false;

			return u1.Equals (u2);
		}

		public static bool operator != (Uri u1, Uri u2)
		{
			if (Object.Equals (u1, null))
				if (Object.Equals (u2, null))
					return false;
				else
					return true;

			return !u1.Equals (u2);
		}

		[DllImport ("librdf")]
		static extern void librdf_free_uri (HandleRef uri);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing is true then dispose
				// of managed resources

				if (handle.Handle != IntPtr.Zero) {
					librdf_free_uri (handle);
					handle = new HandleRef (this, IntPtr.Zero);
				}
				world.RemoveReference ();
				world = null;
				disposed = true;
			}
		}

		public override int GetHashCode ()
		{
			return this.ToString ().GetHashCode ();
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
		static extern IntPtr librdf_uri_to_string (HandleRef uri);

		public override string ToString ()
		{
			IntPtr istr = librdf_uri_to_string (handle);
			return Util.UTF8PtrToString (istr);
		}
	}
}
