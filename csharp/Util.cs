using System;
using System.Runtime.InteropServices;

namespace Redland {

	public class Util {

		[DllImport ("libc", EntryPoint="fopen")]
		public static extern IntPtr fopen (string path, string mode);

		[DllImport ("libc", EntryPoint="fputs")]
		public static extern IntPtr fputs (string path, IntPtr fh);

		[DllImport ("libc", EntryPoint="fclose")]
		public static extern IntPtr fclose (IntPtr fh);

		public static IntPtr StringToHGlobalUTF8 (String s)
		{
			if (s == null)
				return IntPtr.Zero;

			int len = System.Text.Encoding.UTF8.GetByteCount (s);
			byte [] bytes = new byte [len + 1];
			System.Text.Encoding.UTF8.GetBytes (s, 0, s.Length, bytes, 0);
			// set up 0 terminator
			bytes [len] = 0;
			
			IntPtr n = Marshal.AllocHGlobal (len + 1);
			Marshal.Copy (bytes, 0, n, len + 1);

			return n;
		}

		public static String UTF8PtrToString (IntPtr p)
		{
			if (p == IntPtr.Zero)
				return "";

			int len = 0;
			while (Marshal.ReadByte (p, len) != 0)
				len++;
			byte [] bytes = new byte [len];
			Marshal.Copy (p, bytes, 0, len);
			return new String (System.Text.Encoding.UTF8.GetChars (bytes));
		}

	}
}
