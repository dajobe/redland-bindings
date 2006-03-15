//
// QueryResults.cs - Redland Query Results
//
// $Id$
//
// Copyright (C) 2004-2006 David Beckett - http://purl.org/net/dajobe/
// Copyright (C) 2004-2005 University of Bristol - http://www.bristol.ac.uk/
//

using System;
using System.Collections;
using System.Runtime.InteropServices;

namespace Redland {

	public class QueryResults : IWrapper, IEnumerator, IEnumerable, IDisposable {
		private HandleRef handle;

		private Hashtable results;
		private bool disposed = false;
		private bool started = false;
		private World world = Redland.World.AddReference ();

		public HandleRef Handle {
			get { return handle; }
		}

		[DllImport ("librdf")]
		static extern int librdf_query_results_get_bindings_count (HandleRef query_results);

		[DllImport ("librdf")]
		static extern IntPtr librdf_query_results_get_binding_name (HandleRef query_results, int offset);

		[DllImport ("librdf")]
		static extern IntPtr librdf_query_results_get_binding_value (HandleRef query_results, int offset);

		[DllImport ("librdf")]
		static extern int librdf_query_results_next (HandleRef query_results);

		private Hashtable MakeResultsHash ()
		{
			Hashtable h = new Hashtable ();
			int c = librdf_query_results_get_bindings_count (handle);
			for (int i = 0; i < c; i++) {
				IntPtr iname = librdf_query_results_get_binding_name (handle, i);
				String name = Util.UTF8PtrToString (iname);
				IntPtr v = librdf_query_results_get_binding_value (handle, i);
				if (v != IntPtr.Zero) {
					h.Add (name, null);
                                } else {
                                	h.Add (name, new Node (v));
                                }
			}

			return h;
		}



		// IEnumerator implementation
		public object Current {
			get { 
				if (results == null)
					results = MakeResultsHash ();
				return results;
			}
		}

		[DllImport ("librdf")]
		static extern int librdf_query_results_finished (HandleRef query_results);
		public bool MoveNext ()
		{
			if (started) {
				librdf_query_results_next (handle);
			} else {	
				started = true;
			}
			results = null;
			return (librdf_query_results_finished (handle) == 0);
		}

		public void Reset ()
		{
			throw new NotSupportedException ();
		}

		internal QueryResults (IntPtr raw)
		{
			handle = new HandleRef (this, raw);
		}

		// methods only for this class

		[DllImport ("librdf")]
		static extern IntPtr librdf_query_results_as_stream (HandleRef handle);

		public Stream AsStream ()
		{
			IntPtr raw_ret = librdf_query_results_as_stream (handle);
			// FIXME: throw exception if zero?
			return new Stream (raw_ret);
		}

		// These Binding* methods apply to the current position in the
		// result set.

		public Node BindingValue (int offset)
		{
 			IntPtr v = librdf_query_results_get_binding_value (handle, offset);
			if (v != IntPtr.Zero)
				return new Node (v); // do_not_copy=1 FIXME
			else
				return null;
		}

		public string BindingName (int offset) 
		{
 			IntPtr iname = librdf_query_results_get_binding_name (handle, offset);
			return Util.UTF8PtrToString (iname);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_query_results_get_binding_value_by_name (HandleRef query_results, IntPtr name);

		public Node BindingValueByName (string name)
		{
			IntPtr iname = Util.StringToHGlobalUTF8 (name.ToString());
 			IntPtr v = librdf_query_results_get_binding_value_by_name (handle, iname);
			Marshal.FreeHGlobal (iname);
			if (v != IntPtr.Zero)
				return new Node (v); // do_not_copy=1 FIXME
			else
				return null;
		}

		public int BindingsCount ()
		{
			return librdf_query_results_get_bindings_count (handle);
		}

		public IEnumerator GetEnumerator ()
		{
			return this;
		}

		[DllImport ("librdf")]
		static extern void librdf_free_query_results (HandleRef handle);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing is true, dispose of any
				// managed resources
				
				if (disposing) {
					if (results != null) 
						results = null;
				}

				if (handle.Handle != IntPtr.Zero) {
					librdf_free_query_results (handle);
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

		~QueryResults ()
		{
			Dispose (false);
		}
	}
}

