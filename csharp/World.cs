//
// World.cs: Redland Initialization class
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//	Edd Dumbill (edd@usefulinc.com)
//
// (C) 2004, Cesar Lopez Nataren
// Edd Dumbill
//

using System; using System.Runtime.InteropServices;
using System.Threading;
using System.Collections;

namespace Redland {

	class World : IWrapper {

		IntPtr world;

		internal ArrayList errors;
		internal ArrayList warnings;

		public string [] Errors {
			get {
				return (string []) errors.ToArray (typeof (String));
			}
		}
		public string [] Warnings {
			get { return (string []) warnings.ToArray (typeof (String)); }
		}

		public bool ErrorsWaiting {
			get { return errors.Count > 0; }
		}

		public bool WarningsWaiting {
			get { return warnings.Count > 0; }
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_world ();

		internal World ()
		{
			world = librdf_new_world ();
			ClearLog ();
			setupHandlers ();
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

		[DllImport ("librdf")]
		static extern void librdf_free_world (IntPtr world);

		~World ()
		{
			librdf_free_world (world);
		}

		public void ClearLog ()
		{
			errors = new ArrayList ();
			warnings = new ArrayList ();
		}

		// called by anything that can get errors, in order to lock
		// the world and clear the error logs
		public void Enter ()
		{
			Monitor.Enter (this);
			ClearLog ();
		}

		// called by anything that can get errors, in order to unlock
		// the world
		public void Exit ()
		{
			Monitor.Exit (this);
		}

		private delegate int intErrorHandler (IntPtr userdata, IntPtr message, IntPtr args);

		[DllImport ("librdf")]
		static extern void librdf_world_set_error (IntPtr world, IntPtr userdata, Delegate cb);
		[DllImport ("librdf")]
		static extern void librdf_world_set_warning (IntPtr world, IntPtr userdata, Delegate cb);

		private int dispatchError (IntPtr userdata, IntPtr message, IntPtr args)
		{
			errors.Add (Marshal.PtrToStringAuto (message));
			return 1;
		}

		private int dispatchWarning (IntPtr userdata, IntPtr message, IntPtr args)
		{
			warnings.Add (Marshal.PtrToStringAuto (message));
			return 1;
		}

		private void setupHandlers ()
		{
			librdf_world_set_error (world,
					new IntPtr (0), new intErrorHandler (dispatchError));
			librdf_world_set_warning (world,
					new IntPtr (0), new intErrorHandler (dispatchWarning));
		}
	}
}
