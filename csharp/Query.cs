//
// Query.cs - Redland Query class
//
// $Id$
//
// Copyright (C) 2004-2005 David Beckett - http://purl.org/net/dajobe/
// Copyright (C) 2004-2005 University of Bristol - http://www.bristol.ac.uk/
//

using System;
using System.Runtime.InteropServices;

namespace Redland {

	public class Query : IWrapper, IDisposable {
		
		private HandleRef handle;

		private bool disposed = false;
		private World world = Redland.World.AddReference ();

		public static string RDQL = "rdql";

		public HandleRef Handle {
			get { return handle; }
		}

		public Query (string s)
			: this (s, null, RDQL, null)
		{
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_query (HandleRef world, IntPtr query_language, IntPtr uri, IntPtr query_string, IntPtr query_uri);

		public Query (string s, Uri base_uri, string query_language, Uri query_uri)
		{
			IntPtr iql = Util.StringToHGlobalUTF8 (query_language.ToString());
			IntPtr iqs = Util.StringToHGlobalUTF8 (s.ToString());
			IntPtr ibase_uri = IntPtr.Zero;
                        
			if (base_uri != (Uri) null)
				ibase_uri = base_uri.Handle.Handle;

			IntPtr iquery_uri = IntPtr.Zero;

			if (query_uri != (Uri) null)
				iquery_uri = query_uri.Handle.Handle;

			IntPtr query = librdf_new_query (world.Handle, iql, iquery_uri, iqs, ibase_uri);
			handle = new HandleRef (this, query);
			
			Marshal.FreeHGlobal (iql);
			Marshal.FreeHGlobal (iqs);
		}

		[DllImport ("librdf")]
		static extern void librdf_free_query (HandleRef query);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing is true, dispose of managed
				// resources

				if (handle.Handle != IntPtr.Zero) {
					librdf_free_query (handle);
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

		~Query ()
		{
			Dispose (false);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_query_execute (HandleRef query, HandleRef model);

		public QueryResults Execute (Model model)
		{
			IntPtr raw_qr = librdf_query_execute (handle, model.Handle);
			// FIXME: throw exception if raw_qr is zero
			QueryResults qr;
			if (raw_qr != IntPtr.Zero)
				qr = new QueryResults (raw_qr);
			else
				qr = null;
			return qr;
		}
	}
}
