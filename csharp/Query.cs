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
			: this (Redland.World, s, null, RDQL, null)
		{
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_query (IntPtr world, IntPtr query_language, IntPtr uri, IntPtr query_string, IntPtr query_uri);

		private Query (World world, string s, Uri base_uri, string query_language, Uri query_uri)
		{
			IntPtr iql = Marshal.StringToHGlobalAuto (query_language.ToString());
			IntPtr iqs = Marshal.StringToHGlobalAuto (s.ToString());
			IntPtr ibase_uri = IntPtr.Zero;
                        
			if (base_uri != (Uri) null)
				ibase_uri = base_uri.Handle;
                        
			IntPtr iquery_uri = IntPtr.Zero;
                        
			if (query_uri != (Uri) null)
				iquery_uri = query_uri.Handle;
                        
			query = librdf_new_query (Redland.World.Handle, iql, iquery_uri, iqs, ibase_uri);
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
