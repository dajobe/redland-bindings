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

	public class Query : IWrapper {
		
		IntPtr query;

		public IntPtr Handle {
			get { return query; }
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_query (IntPtr world);

		public Query ()
		{
			query = librdf_new_query (Redland.World.Handle);
		}

		public Query (string s)
			: this (Redland.World, s, null, "rdql")
		{
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_query (IntPtr world, IntPtr query_language, IntPtr uri, IntPtr query_string);

		private Query (World world, string s, Uri uri, string query_language)
		{
			IntPtr iql = Marshal.StringToHGlobalAuto (query_language.ToString());
			IntPtr iqs = Marshal.StringToHGlobalAuto (s.ToString());
			query = librdf_new_query (Redland.World.Handle, iql, uri.Handle, iqs);
                        Marshal.FreeHGlobal (iql);
                        Marshal.FreeHGlobal (iqs);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_query_execute (IntPtr query, IntPtr model);

		public QueryResults Execute (Model model)
		{
			IntPtr raw_qr = librdf_query_execute (query, model.Handle);
			QueryResults qr = new QueryResults (this,raw_qr);
			return qr;
		}


	}
}
