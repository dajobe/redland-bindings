//
// Storage.cs: Redland Statement Storage class
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren
//

using System;
using System.Runtime.InteropServices;

namespace Rdf {

	public class Storage : IWrapper {

		IntPtr storage;

		public IntPtr Handle {
			get { return storage; }
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_storage (IntPtr world, string storage_name, string name, string options);

		private Storage (World world, string storage_name, string name, string options)
		{
			storage = librdf_new_storage (world.Handle, storage_name, name, options);
		}

		public Storage (string storage_name, string name, string options)
			: this (Redland.World, storage_name, name, options)
		{
		}
	}
}
