//
// Statement.cs: Redland Statement (triple) class.
//
// $Id$
//
// The main means of manipulating statement is by the subject, predicate
// and object properties.
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren
//

using System;
using System.Runtime.InteropServices;

namespace Redland {

	public class Statement : IWrapper, IDisposable {
		
		private HandleRef handle;

		private bool disposed = false;
		private World world = Redland.World.AddReference ();

		public HandleRef Handle {
			get { return handle; }
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_statement (HandleRef world);
		
		public Statement ()
		{
			handle = new HandleRef (this, librdf_new_statement (world.Handle));
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_statement_from_nodes (HandleRef world, IntPtr subject, IntPtr predicate, IntPtr obj);

        [DllImport ("librdf")]
		static extern IntPtr librdf_new_node_from_node (HandleRef node);

		public Statement (Node subject, Node predicate, Node obj)
		{
			IntPtr subj, pred, o;
			subj = pred = o = IntPtr.Zero;

 			// Console.WriteLine ("Making Statement from {0} {1} {2}", subject.ToString(), predicate.ToString(), obj.ToString());
			
			if (subject != null)
				subj = librdf_new_node_from_node (subject.Handle);
			if (predicate != null)
				pred = librdf_new_node_from_node (predicate.Handle);
			if (obj != null)
				o = librdf_new_node_from_node (obj.Handle);
			
			//  statement takes mem ownership of underlying nodes
			IntPtr stm = librdf_new_statement_from_nodes (world.Handle, subj, pred, o);
			handle = new HandleRef (this, stm);
 			// Console.WriteLine ("New Statement is {0}", stm.ToString());
		}

		[DllImport ("librdf")]
		static extern void librdf_statement_set_subject (HandleRef stm, HandleRef node);
		[DllImport ("librdf")]
		static extern IntPtr librdf_statement_get_subject (HandleRef stm);

		public Node Subject {
			get {
				return new Node (librdf_statement_get_subject (handle));
			}
			set { 
				librdf_statement_set_subject (handle, value.Handle);
			}
		}

		[DllImport ("librdf")]
		static extern void librdf_statement_set_predicate (HandleRef stm, HandleRef node);
		[DllImport ("librdf")]
		static extern IntPtr librdf_statement_get_predicate (HandleRef stm);

		public Node Predicate {
			get {
				return new Node (librdf_statement_get_predicate (handle));
			}
			set { 
				librdf_statement_set_predicate (handle, value.Handle);
			}
		}

		[DllImport ("librdf")]
		static extern void librdf_statement_set_object (HandleRef statement, HandleRef node);
		[DllImport ("librdf")]
		static extern IntPtr librdf_statement_get_object (HandleRef stm);

		public Node Object {
			get {
				return new Node (librdf_statement_get_object (handle));
			}
			set {
				librdf_statement_set_object (handle, value.Handle);
			}
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_statement_from_statement (IntPtr stm);

		internal Statement (IntPtr raw)
		{
			IntPtr stm = librdf_new_statement_from_statement (raw);
			handle = new HandleRef (this, stm);
		}

		[DllImport ("librdf")]
		static extern void librdf_free_statement (HandleRef statement);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing is true, then dispose of 
				// managed resources
				
				if (handle.Handle != IntPtr.Zero) {
					librdf_free_statement (handle);
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

		~Statement ()
		{
			Dispose (false);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_statement_to_string (HandleRef stm);

		public override string ToString ()
		{
			IntPtr istr = librdf_statement_to_string (handle);
			return Util.UTF8PtrToString (istr);
		}
	}
}
