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
//           Edd Dumbill
//

using System;
using System.Runtime.InteropServices;
using System.Threading;
using System.Collections;

namespace Redland {

	class World : IWrapper {

		IntPtr world;

		internal ArrayList messages;

		LogLevel level;

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
		static extern void librdf_world_set_logger (IntPtr world,
				IntPtr userdata, Delegate cb);

		internal World ()
		{
			world = librdf_new_world ();
			ClearLog ();
			mhandler = new MessageHandler (dispatchMessage);
			librdf_world_set_logger (world, new IntPtr (0), mhandler);
			level = LogLevel.Warn;
		}

		public LogLevel LogLevel {
			get {
				return level;
			}
			set {
				level = value;
			}
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
