//
// Serializer.cs:
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren 
//

using System;
using System.Runtime.InteropServices;

namespace Rdf {

	public class Serializer : IWrapper {

		IntPtr serializer;

		public IntPtr Handle {
			get { return serializer; }
		}

		public Serializer (string name, string mime_type, Uri type_uri)
			: this (Redland.World, name, mime_type, type_uri)
		{
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_serializer (IntPtr world, string name, string mime_type, IntPtr type_uri);

		private Serializer (World world, string name, string mime_type, Uri type_uri)
		{
			if (world == null)
				if (type_uri == null)
					serializer = librdf_new_serializer (IntPtr.Zero, name, mime_type, IntPtr.Zero);
				else
					serializer = librdf_new_serializer (IntPtr.Zero, name, mime_type, type_uri.Handle);
			else if (type_uri == null)
				serializer = librdf_new_serializer (world.Handle, name, mime_type, IntPtr.Zero);
			else
				serializer = librdf_new_serializer (world.Handle, name, mime_type, type_uri.Handle);
		}

		[DllImport ("librdf")]
		static extern int librdf_serializer_serialize_model (IntPtr serializer, IntPtr file, IntPtr base_uri, IntPtr model);

		public int SerializeModel (IntPtr file, Uri base_uri, Model model)
		{
			return librdf_serializer_serialize_model (serializer, file, base_uri.Handle, model.Handle);
		}
	}
}
