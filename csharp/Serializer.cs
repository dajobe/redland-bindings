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

		IntPtr serializer = IntPtr.Zero;

		bool disposed = false;

		public IntPtr Handle {
			get { return serializer; }
		}

		public Serializer (string name, string mime_type, Uri type_uri)
			: this (Redland.World, name, mime_type, type_uri)
		{
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_serializer (IntPtr world, IntPtr name, IntPtr mime_type, IntPtr type_uri);

		private Serializer (World world, string name, string mime_type, Uri type_uri)
		{
			IntPtr iname = Marshal.StringToHGlobalAuto (name);
			IntPtr imime_type = Marshal.StringToHGlobalAuto (mime_type);
			if (world == null)
				if ((Object)type_uri == null)
					serializer = librdf_new_serializer (IntPtr.Zero, iname, imime_type, IntPtr.Zero);
				else
					serializer = librdf_new_serializer (IntPtr.Zero, iname, imime_type, type_uri.Handle);
			else if ((Object)type_uri == null)
				serializer = librdf_new_serializer (world.Handle, iname, imime_type, IntPtr.Zero);
			else
				serializer = librdf_new_serializer (world.Handle, iname, imime_type, type_uri.Handle);
                        Marshal.FreeHGlobal (iname);
                        Marshal.FreeHGlobal (imime_type);

		}

		[DllImport ("librdf")]
		static extern void librdf_free_serializer (IntPtr serializer);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing is true, then dispose of managed
				// resources

				if (serializer != IntPtr.Zero) {
					librdf_free_serializer (serializer);
					serializer = IntPtr.Zero;
				}
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
		static extern int librdf_serializer_serialize_model (IntPtr serializer, IntPtr file, IntPtr base_uri, IntPtr model);

		public int SerializeModel (IntPtr file, Uri base_uri, Model model)
		{
			// FIXME: throw exceptions instead of using ret code?
			return librdf_serializer_serialize_model (serializer, file, base_uri.Handle, model.Handle);
		}


		[DllImport ("librdf")]
		static extern int librdf_serializer_serialize_model_to_file (IntPtr serializer, IntPtr name, IntPtr base_uri, IntPtr model);

		public int SerializeModel (string name, Uri base_uri, Model model)
		{
			IntPtr iname = Marshal.StringToHGlobalAuto (name);
			int ret = librdf_serializer_serialize_model_to_file (serializer, iname, base_uri.Handle, model.Handle);
			Marshal.FreeHGlobal (iname);
			// FIXME: throw exceptions instead of using ret code?
			return ret;
}
	}
}
