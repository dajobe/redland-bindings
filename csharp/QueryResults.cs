//
// QueryResults.cs - Redland Query Results
//
// $Id$
//
// Copyright (C) 2004 David Beckett - http://purl.org/net/dajobe/
// Institute for Learning and Research Technology - http://www.ilrt.bris.ac.uk/
// University of Bristol - http://www.bristol.ac.uk/
//

using System;
using System.Collections;
using System.Runtime.InteropServices;

namespace Redland {

	public class QueryResults : IWrapper, IEnumerator {
		Query query;
		IntPtr query_results;
		int pos = 0;

		public IntPtr Handle {
			get { return query_results; }
		}

		[DllImport ("librdf")]
		static extern int librdf_query_results_finished (IntPtr query_results);
		
		[DllImport ("librdf")]
		static extern int librdf_query_results_get_bindings_count (IntPtr query_results);

		[DllImport ("librdf")]
		static extern IntPtr librdf_query_results_get_binding_name (IntPtr query_results, int offset);

		[DllImport ("librdf")]
		static extern IntPtr librdf_query_results_get_binding_value (IntPtr query_results, int offset);

		[DllImport ("librdf")]
		static extern int librdf_query_results_next (IntPtr query_results);

		private Hashtable MakeResultsHash ()
		{
			Hashtable h = new Hashtable();
                        int c= librdf_query_results_get_bindings_count(query_results);
                        for (int i = 0; i < c; i++) {
                        	IntPtr iname = librdf_query_results_get_binding_name(query_results,i);
                        	String name=Marshal.PtrToStringAuto(iname);
                                IntPtr v = librdf_query_results_get_binding_value(query_results,i);
                                h.Add(name, new Node(v));
                        }
                        
                        return h;
                }

		// IEnumerator implementation
		public object Current {
			get { 
				Hashtable h=MakeResultsHash();
				return h;						 
			}
		}

		public bool MoveNext ()
		{
			int r =	librdf_query_results_next (query_results);
			if (r != 0)
				return false;
			else
				return true;
		}

		public void Reset ()
		{
			throw new NotSupportedException ();
		}

		[DllImport ("librdf")]
		static extern int libdf_query_results_finished (IntPtr query_results);

		public bool End {
			get {
				int r = librdf_query_results_finished (query_results);
				if (r != 0)
					return true;
				else
					return false;
			}
		}

		public QueryResults (Query query, IntPtr query_results)
		{
			this.query = query;
			this.query_results = query_results;
		}

        	// methods only for this class

		[DllImport ("librdf")]
                static extern IntPtr librdf_query_results_as_stream(IntPtr query_results);

        	public Stream AsStream () {
                	IntPtr raw_ret = librdf_query_results_as_stream(query_results);
			Stream stream = new Stream (raw_ret);
			return stream;
                }


		public Node BindingValue (int offset) {
 			IntPtr v = librdf_query_results_get_binding_value(query_results,offset);
			Node n = new Node(v); // do_not_copy=1 FIXME
                        return n;
		}

		public string BindingName (int offset) {
 			IntPtr iname = librdf_query_results_get_binding_name(query_results,offset);
			String name=Marshal.PtrToStringAuto(iname);
			return name;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_query_results_get_binding_value_by_name (IntPtr query_results, IntPtr name);

		public Node BindingValueByName (string name) {
			IntPtr iname = Marshal.StringToHGlobalAuto (name.ToString());
 			IntPtr v = librdf_query_results_get_binding_value_by_name(query_results,iname);
			Node n = new Node(v); // do_not_copy=1 FIXME
                        Marshal.FreeHGlobal (iname);
                        return n;
		}

		public int BindingsCount () {
			return librdf_query_results_get_bindings_count(query_results);
		}



	}
}

