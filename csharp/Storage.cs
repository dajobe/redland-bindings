//
// Storage.cs: Redland Statement Storage class
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

	public class Storage : IWrapper, IDisposable {
		
		private HandleRef handle;

		private bool disposed = false;
		private World world = Redland.World.AddReference ();

		public HandleRef Handle {
			get { return handle; }
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_storage (HandleRef world, IntPtr storage_name, IntPtr name, IntPtr options);

		public Storage (string storage_name, string name, string options)
		{
			IntPtr istorage_name = Util.StringToHGlobalUTF8 (storage_name);
			IntPtr iname = Util.StringToHGlobalUTF8 (name);
			IntPtr ioptions = Util.StringToHGlobalUTF8 (options);
			IntPtr storage = librdf_new_storage (world.Handle, istorage_name, iname, ioptions);
			handle = new HandleRef (this, storage);
			Marshal.FreeHGlobal (istorage_name);
			Marshal.FreeHGlobal (iname);
			Marshal.FreeHGlobal (ioptions);
		}

		[DllImport ("librdf")]
		static extern void librdf_free_storage (HandleRef storage);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing is true, then dispose of
				// managed resources

				if (handle.Handle != IntPtr.Zero) {
					librdf_free_storage (handle);
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

		~Storage ()
		{
			Dispose (false);
		}
	}
}
