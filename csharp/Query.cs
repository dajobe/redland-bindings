//
// Query.cs - Redland Query class
//
// $Id$
//
// Copyright (C) 2004 David Beckett - http://purl.org/net/dajobe/
// Institute for Learning and Research Technology - http://www.ilrt.bris.ac.uk/
// University of Bristol - http://www.bristol.ac.uk/
//

using System;
using System.Runtime.InteropServices;

namespace Redland {

	public class Query : IWrapper, IDisposable {
		
		IntPtr query = IntPtr.Zero;
		bool disposed = false;

		public static string RDQL = "rdql";

		public IntPtr Handle {
			get { return query; }
		}

		public Query (string s)
			: this (Redland.World, s, null, RDQL)
		{
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_query (IntPtr world, IntPtr query_language, IntPtr uri, IntPtr query_string);

		private Query (World world, string s, Uri uri, string query_language)
		{
			IntPtr iql = Marshal.StringToHGlobalAuto (query_language.ToString());
			IntPtr iqs = Marshal.StringToHGlobalAuto (s.ToString());
			IntPtr iuri = IntPtr.Zero;
                        
			if (uri != (Uri) null)
				iuri = uri.Handle;
                        
			query = librdf_new_query (Redland.World.Handle, iql, iuri, iqs);
			Marshal.FreeHGlobal (iql);
			Marshal.FreeHGlobal (iqs);
		}

		[DllImport ("librdf")]
		static extern void librdf_free_query (IntPtr query);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing is true, dispose of managed
				// resources

				if (query != IntPtr.Zero) {
					librdf_free_query (query);
					query = IntPtr.Zero;
				}
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
		static extern IntPtr librdf_query_execute (IntPtr query, IntPtr model);

		public QueryResults Execute (Model model)
		{
			IntPtr raw_qr = librdf_query_execute (query, model.Handle);
			// FIXME: throw exception if raw_qr is zero
			QueryResults qr = new QueryResults (raw_qr);
			return qr;
		}
	}
}
