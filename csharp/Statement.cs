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
		
		IntPtr stm = IntPtr.Zero;

		bool disposed = false;

		public IntPtr Handle {
			get { return stm; }
		}

		public Statement ()
			: this (Redland.World)
		{
		}

		public Statement (Node subject, Node predicate, Node obj)
			: this (Redland.World, subject, predicate, obj)
		{
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_statement (IntPtr world);
		
		private Statement (World world)
		{
			stm = librdf_new_statement (world.Handle);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_statement_from_nodes (IntPtr world, IntPtr subject, IntPtr predicate, IntPtr obj);

		private Statement (World world, Node subject, Node predicate, Node obj)
		{
			IntPtr subj, pred, o;
			subj = pred = o = IntPtr.Zero;

 			// Console.WriteLine ("Making Statement from {0} {1} {2}", subject.ToString(), predicate.ToString(), obj.ToString());
			
			if (subject != null)
				subj = new Node (subject).Handle;
			if (predicate != null)
				pred = new Node (predicate).Handle;
			if (obj != null)
				o = new Node (obj).Handle;
			stm = librdf_new_statement_from_nodes (world.Handle, subj, pred, o);
 			// Console.WriteLine ("New Statement is {0}", stm.ToString());
		}

		[DllImport ("librdf")]
		static extern void librdf_statement_set_subject (IntPtr stm, IntPtr node);
		[DllImport ("librdf")]
		static extern IntPtr librdf_statement_get_subject (IntPtr stm);

		public Node Subject {
			get {
				return new Node (librdf_statement_get_subject (Handle));
			}
			set { 
				librdf_statement_set_subject (Handle, value.Handle);
			}
		}

		[DllImport ("librdf")]
		static extern void librdf_statement_set_predicate (IntPtr stm, IntPtr node);
		[DllImport ("librdf")]
		static extern IntPtr librdf_statement_get_predicate (IntPtr stm);

		public Node Predicate {
			get {
				return new Node (librdf_statement_get_predicate (Handle));
			}
			set { 
				librdf_statement_set_predicate (Handle, value.Handle);
			}
		}

		[DllImport ("librdf")]
		static extern void librdf_statement_set_object (IntPtr statement, IntPtr node);
		[DllImport ("librdf")]
		static extern IntPtr librdf_statement_get_object (IntPtr stm);

		public Node Object {
			get {
				return new Node (librdf_statement_get_object (Handle));
			}
			set {
				librdf_statement_set_object (stm, value.Handle);
			}
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_statement_from_statement (IntPtr world);

		internal Statement (IntPtr raw)
		{
			stm = librdf_new_statement_from_statement (raw);
		}

		[DllImport ("librdf")]
		static extern void librdf_free_statement (IntPtr statement);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing is true, then dispose of 
				// managed resources
				
				if (stm != IntPtr.Zero) {
					librdf_free_statement (stm);
					stm = IntPtr.Zero;
				}
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
		static extern IntPtr librdf_statement_to_string (IntPtr stm);

		public override string ToString ()
		{
			IntPtr istr = librdf_statement_to_string (stm);
			return Marshal.PtrToStringAuto (istr);
		}
	}
}
