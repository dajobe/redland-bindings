//
// World.cs: Redland Initialization class
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren
//

using System;
using System.Runtime.InteropServices;

namespace Redland {

	class World : IWrapper {

		IntPtr world;

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_world ();

		internal World ()
		{
			world = librdf_new_world ();
		}

		[DllImport ("librdf")]
		static extern void librdf_world_open (IntPtr world);

		internal void Open ()
		{
			librdf_world_open (world);
		}

		public  IntPtr Handle {
			get { return world; }
		}
	}
}
