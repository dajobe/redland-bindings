//
// Serializer.cs: Redland Serialize to syntax class
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

	public class Serializer : IWrapper, IDisposable {

		private HandleRef handle;

		private bool disposed = false;
		private World world = Redland.World.AddReference ();

		public HandleRef Handle {
			get { return handle; }
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_serializer (HandleRef world, IntPtr name, IntPtr mime_type, IntPtr type_uri);

		public Serializer (string name, string mime_type, Uri type_uri)
		{
			IntPtr iname = Util.StringToHGlobalUTF8 (name);
			IntPtr imime_type = Util.StringToHGlobalUTF8 (mime_type);
			IntPtr serializer = IntPtr.Zero;

			if (type_uri == null)
				serializer = librdf_new_serializer 
					(world.Handle, iname, imime_type, IntPtr.Zero);
			else
				serializer = librdf_new_serializer
					(world.Handle, iname, imime_type, type_uri.Handle.Handle);

			handle = new HandleRef (this, serializer);
			Marshal.FreeHGlobal (iname);
			Marshal.FreeHGlobal (imime_type);
		}

		[DllImport ("librdf")]
		static extern void librdf_free_serializer (HandleRef serializer);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing is true, then dispose of managed
				// resources

				if (handle.Handle != IntPtr.Zero) {
					librdf_free_serializer (handle);
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

		~Serializer ()
		{
			Dispose (false);
		}

		[DllImport ("librdf")]
		static extern int librdf_serializer_serialize_model (HandleRef serializer, IntPtr file, HandleRef base_uri, HandleRef model);

		public int SerializeModel (IntPtr file, Uri base_uri, Model model)
		{
			// FIXME: throw exceptions instead of using ret code?
			return librdf_serializer_serialize_model (handle, file, base_uri.Handle, model.Handle);
		}


		[DllImport ("librdf")]
		static extern int librdf_serializer_serialize_model_to_file (HandleRef serializer, IntPtr name, HandleRef base_uri, HandleRef model);

		public int SerializeModel (string name, Uri base_uri, Model model)
		{
			IntPtr iname = Util.StringToHGlobalUTF8 (name);
			int ret = librdf_serializer_serialize_model_to_file (handle, iname, base_uri.Handle, model.Handle);
			Marshal.FreeHGlobal (iname);
			// FIXME: throw exceptions instead of using ret code?
			return ret;
}
	}
}
