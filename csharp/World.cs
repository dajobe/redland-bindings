//
// World.cs: Redland Initialization class
//
// $Id$
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//	Edd Dumbill (edd@usefulinc.com)
//
// (C) 2004, Cesar Lopez Nataren
// (C) 2004-5 Edd Dumbill
//

using System;
using System.Runtime.InteropServices;
using System.Threading;
using System.Collections;

namespace Redland {

	public class WorldException : Exception {
		public WorldException (String s): base (s) { }
	}

	class World : IWrapper {

		private static HandleRef handle;

		internal ArrayList messages;
		private LogLevel level;

		private static int refs = 0;
		private static bool initialized = false;

		// HOW WORLD'S LIFETIME IS MANAGED -- Edd Dumbill, 2005-01-05
		//
		// It is necessary to keep an instance of the C world around for
		// as long as any Redland object needs it.  This includes in their
		// destructors.  Unfortunately, when all references to objects and
		// any 'inner' objects referred to within are liable to finalization,
		// the .NET finalizer makes no distinction about order.  This means
		// that if the C-world were closed from ~World() then some of the
		// other object finalizers would crash as the C world would close
		// before their own C based destructors (which need world) could run.
		// 
		// To get around this, we detach the destruction of the C world from
		// the .NET World object lifecycle, and use a single static reference
		// to it, with manual ref-counting.  Each client object uses
		// AddReference at construct-time and RemoveReference in Dispose().
		// This includes the .NET World object.
		//
		// When RemoveReference makes the ref count hit zero, the C-world is
		// destroyed.  To ensure this only happens at the end of the run, the
		// World object itself performs an AddReference on construct and 
		// RemoveReference on destruct.

		public LogMessage [] Messages {
			get {
				return (LogMessage []) messages.ToArray (typeof (LogMessage));
			}
		}

		private delegate int MessageHandler (IntPtr userdata, IntPtr message);
		private MessageHandler mhandler;

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_world ();
		[DllImport ("librdf")]
		static extern void librdf_world_set_logger (HandleRef world,
				IntPtr userdata, Delegate cb);

		internal World ()
		{
			if (! initialized) {
				handle = new HandleRef (this, librdf_new_world ());
				ClearLog ();
				mhandler = new MessageHandler (dispatchMessage);
				librdf_world_set_logger (handle, new IntPtr (0), mhandler);
				level = LogLevel.Warn;
				// logger set up, now let's open the world
				Open ();
				initialized = true;
				AddReference ();
			} else {
				throw new WorldException ("Can only make one World");
			}
		}

		~World () {
			RemoveReference ();
		}

		public LogLevel LogLevel {
			get { return level; }
			set { level = value; }
		}

		[DllImport ("librdf")]
		static extern void librdf_world_open (HandleRef world);

		static internal void Open ()
		{
			librdf_world_open (handle);
		}

		public HandleRef Handle {
			get { return handle; }
		}

		public World AddReference () {
			refs++;
			return this;
		}

		public void RemoveReference () {
			if (refs == 0) {
				throw new WorldException ("Attempt to remove reference from World when no references held");
			}
			refs--;
			if (refs == 0) {
				FreeWorld ();
			}
		}

		[DllImport ("librdf")]
		static extern void librdf_free_world (HandleRef world);

		protected static void FreeWorld ()
		{
			if (refs > 0)
				throw new WorldException ("Attempt to free world when references still held");

			if (handle.Handle != IntPtr.Zero) {
				librdf_free_world (handle);
				handle = new HandleRef (handle.Wrapper, IntPtr.Zero);
			}
		}

		public void ClearLog ()
		{
			messages = new ArrayList ();
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

		// handle incoming log messages
		private int dispatchMessage (IntPtr userdata, IntPtr message)
		{
			LogMessage lm = new LogMessage (message);
			if (lm.Level >= level)
				messages.Add (lm);
			return 1;
		}
	}
}
