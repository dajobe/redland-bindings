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

		IntPtr storage = IntPtr.Zero;

		bool disposed = false;

		public IntPtr Handle {
			get { return storage; }
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_storage (IntPtr world, IntPtr storage_name, IntPtr name, IntPtr options);

		private Storage (World world, string storage_name, string name, string options)
		{
			IntPtr istorage_name = Marshal.StringToHGlobalAuto (storage_name);
			IntPtr iname = Marshal.StringToHGlobalAuto (name);
			IntPtr ioptions = Marshal.StringToHGlobalAuto (options);
			storage = librdf_new_storage (world.Handle, istorage_name, iname, ioptions);
			Marshal.FreeHGlobal (istorage_name);
			Marshal.FreeHGlobal (iname);
			Marshal.FreeHGlobal (ioptions);
		}

		public Storage (string storage_name, string name, string options)
			: this (Redland.World, storage_name, name, options)
		{
		}

		[DllImport ("librdf")]
		static extern void librdf_free_storage (IntPtr storage);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing is true, then dispose of
				// managed resources

				if (storage != IntPtr.Zero) {
					librdf_free_storage (storage);
					storage = IntPtr.Zero;
				}

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
