//
// Model.cs: Redland Model
//
// $Id$
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren
//

using System;
using System.Runtime.InteropServices;
using System.Collections;

namespace Redland {

	public class Model : IWrapper, IDisposable {

		private Storage storage;
		private HandleRef handle;

		private bool disposed = false;
		private World world = Redland.World.AddReference ();

		public HandleRef Handle {
			get { return handle; }
		}

		public Model (Storage storage)
			: this (storage, null)
		{			
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_model (HandleRef world, HandleRef storage, IntPtr options);

		public Model (Storage storage, string options)
		{
			IntPtr ioptions = Util.StringToHGlobalUTF8 (options);
			IntPtr model = librdf_new_model (world.Handle, storage.Handle, ioptions);
			Marshal.FreeHGlobal (ioptions);
			handle = new HandleRef (this, model);
			// keep a reference around to storage so it doesn't
			// get destroyed before us
			this.storage = storage;
		}

		[DllImport ("librdf")]
		static extern void librdf_free_model (HandleRef model);

		protected void Dispose (bool disposing)
		{
			// Console.WriteLine ("Dispose ({0})", disposing);
			if (! disposed) {
				// if 'disposing' is true, then any managed
				// objects should be disposed here.

				if (handle.Handle != IntPtr.Zero) {
					librdf_free_model (handle);
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

		~Model ()
		{
			//Console.WriteLine ("Destructor");
			Dispose (false);
			//Console.WriteLine ("Destructor done");
		}

		[DllImport ("librdf")]
		static extern int librdf_model_add_statement (HandleRef model, HandleRef statement);

		public int AddStatement (Statement stm)
		{
			return librdf_model_add_statement (handle, stm.Handle);
		}

		[DllImport ("librdf")]
		static extern int librdf_model_context_add_statement (HandleRef model, HandleRef context, HandleRef stm);

		public int AddStatement (Statement stm, Node context)
		{
			return librdf_model_context_add_statement (handle, context.Handle, stm.Handle);
		}

		[DllImport ("librdf")]
		static extern void librdf_model_print (HandleRef model, IntPtr fh);

		public void Print (IntPtr fh)
		{
			librdf_model_print (handle, fh);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_find_statements (HandleRef model, HandleRef stm);

		public Stream FindStatements (Statement stm)
		{
			IntPtr raw_ret = librdf_model_find_statements (handle, stm.Handle);
			return new Stream (raw_ret);
		}
		
		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_source (HandleRef model, HandleRef arc, HandleRef target);

		public Node GetSource (Node arc, Node target)
		{
			Node r;
			IntPtr raw_ret = librdf_model_get_source (handle, arc.Handle, target.Handle);
			if (raw_ret != IntPtr.Zero)
				r = new Node (raw_ret);
			else
				r = null;
			return r;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_sources (HandleRef model, HandleRef arc, HandleRef target);

		public Iterator GetSources (Node arc, Node target)
		{
			IntPtr raw_ret = librdf_model_get_sources (handle, arc.Handle, target.Handle);
			// FIXME: throw exception if raw_ret == IntPtr.Zero?
			Iterator it = new Iterator (raw_ret);
			return it;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_arc (HandleRef model, HandleRef source, HandleRef target);

		public Node GetPredicate (Node source, Node target)
		{
			Node r;
			IntPtr raw_ret = librdf_model_get_arc (handle, source.Handle, target.Handle);
			if (raw_ret != IntPtr.Zero)
				r = new Node (raw_ret);
			else
				r = null;
			return r;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_target (HandleRef model, HandleRef source, HandleRef arc);

		public Node GetTarget (Node source, Node arc)
		{
			IntPtr raw_ret = librdf_model_get_target (handle, source.Handle, arc.Handle);
			Node r;
			if (raw_ret != IntPtr.Zero)
				r = new Node (raw_ret);
			else
				r = null;
			return r;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_targets (HandleRef model, HandleRef source, HandleRef arc);

		public Iterator GetTargets (Node source, Node arc)
		{
			IntPtr raw_ret = librdf_model_get_targets (handle, source.Handle, arc.Handle);
			// FIXME: throw exception if raw_ret is zero?
			Iterator it = new Iterator (raw_ret);
			return it;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_arcs (HandleRef model, HandleRef source, HandleRef target);

		public Iterator GetPredicates (Node source, Node target)
		{
			IntPtr raw_ret = librdf_model_get_arcs (handle, source.Handle, target.Handle);
			// FIXME: throw exception if raw_ret is zero?
			Iterator it = new Iterator (raw_ret);
			return it;
		}

		[DllImport ("librdf")]
		static extern int librdf_model_contains_statement (HandleRef model, HandleRef statement);

		public bool Contains (Statement stm)
		{
			int r = librdf_model_contains_statement (handle, stm.Handle);
			return (r != 0);
		}

		[DllImport ("librdf")]
		static extern int librdf_model_remove_statement (HandleRef model, HandleRef statement);

		public int Remove (Statement statement)
		{
			return librdf_model_remove_statement (handle, statement.Handle);
		}

		[DllImport ("librdf")]
		static extern int librdf_model_context_remove_statement (HandleRef handel, HandleRef context, HandleRef statement);

		public int Remove (Statement stm, Node context)
		{
			return librdf_model_context_remove_statement (handle, context.Handle, stm.Handle);
		}

		[DllImport ("librdf")]
		static extern int librdf_model_size (HandleRef model);

		public int Size {
			get { return librdf_model_size (handle); }
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_as_stream (HandleRef model);

		public Stream ToStream ()
		{
			IntPtr raw_ret = librdf_model_as_stream (handle);
			// FIXME: throw exception if raw_ret == zero?
			Stream stream = new Stream (raw_ret);
			return stream;
		}

		[DllImport ("librdf")]
		static extern int librdf_model_add_statements (HandleRef model, HandleRef statement_stream);

		public int AddStatements (Stream stream)
		{
			return librdf_model_add_statements (handle, stream.Handle);
		}

		[DllImport ("librdf")]
		static extern void librdf_model_sync (HandleRef model);

		public void Sync ()
		{
			librdf_model_sync (handle);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_contexts (HandleRef model);

		public Iterator GetContexts ()
		{
			IntPtr raw_ret = librdf_model_get_contexts (handle);
			// FIXME: throw exception if raw_ret == zero?
			Iterator it = new Iterator (raw_ret);
			return it;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_feature (HandleRef model, HandleRef uri);

		public Node GetFeature (Uri uri)
		{
			IntPtr raw_ret = librdf_model_get_feature (handle, uri.Handle);
			Node r;
			if (raw_ret != IntPtr.Zero)
				r = new Node (raw_ret);
			else
				r = null;
			return r;
		}

		[DllImport ("librdf")]
		static extern void librdf_model_set_feature (HandleRef model, HandleRef uri, HandleRef value);

		public void SetFeature (Uri uri, Node value)
		{
			librdf_model_set_feature (handle, uri.Handle, value.Handle);
		}

		[DllImport ("librdf")]
		static extern void librdf_model_load (HandleRef model, HandleRef uri, IntPtr mime_type, HandleRef type_uri);

		public void Load (Uri uri, String mime_type, Uri type_uri)
		{
			IntPtr imime = Util.StringToHGlobalUTF8 (mime_type.ToString());
			librdf_model_load (handle, uri.Handle, imime, type_uri.Handle);
                        Marshal.FreeHGlobal (imime);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_query_execute (HandleRef model, HandleRef query);

		public QueryResults Execute (Query query)
		{
			IntPtr raw_ret = librdf_model_query_execute (handle, query.Handle);
			// FIXME: throw exception if raw_ret == zero?
			QueryResults qr = new QueryResults (raw_ret);
			return qr;
		}

	}
}
