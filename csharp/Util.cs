using System;
using System.Runtime.InteropServices;

namespace Rdf {

	public class Util {

		[DllImport ("libc", EntryPoint="fopen")]
		public static extern IntPtr fopen (string path, string mode);

		[DllImport ("libc", EntryPoint="fputs")]
		public static extern IntPtr fputs (string path, IntPtr fh);

		[DllImport ("libc", EntryPoint="fclose")]
		public static extern IntPtr fclose (IntPtr fh);

	}
}
