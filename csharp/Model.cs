//
// Model.cs:
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

	public class Model : IWrapper {

		IntPtr model;

		public IntPtr Handle {
			get { return model; }
		}

		public Model (Storage storage)
			: this (Redland.World, storage, null)
		{			
		}

		public Model (Storage storage, string options)
			: this (Redland.World, storage, options)
		{
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_model (IntPtr world, IntPtr storage, IntPtr options);

		internal Model (World world, Storage storage, string options)
		{
			IntPtr ioptions = Marshal.StringToHGlobalAuto (options);
			model = librdf_new_model (world.Handle, storage.Handle, ioptions);
                        Marshal.FreeHGlobal (ioptions);
		}

		[DllImport ("librdf")]
		static extern int librdf_model_add_statement (IntPtr model, IntPtr statement);

		public int AddStatement (Statement stm)
		{
			return librdf_model_add_statement (model, stm.Handle);
		}

		[DllImport ("librdf")]
		static extern int librdf_model_context_add_statement (IntPtr model, IntPtr context, IntPtr stm);

		public int AddStatement (Statement stm, Node context)
		{
			return librdf_model_context_add_statement (model, context.Handle, stm.Handle);
		}

		[DllImport ("librdf")]
		static extern void librdf_model_print (IntPtr model, IntPtr fh);

		public void Print (IntPtr fh)
		{
			librdf_model_print (model, fh);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_find_statements (IntPtr model, IntPtr stm);

		public Stream FindStatements (Statement stm)
		{
			IntPtr raw_ret = librdf_model_find_statements (model, stm.Handle);
			Stream stream = new Stream (raw_ret);
			return stream;
		}
		
		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_source (IntPtr model, IntPtr arc, IntPtr target);

		public Node GetSource (Node arc, Node target)
		{
			Node r;
			IntPtr raw_ret = librdf_model_get_source (model, arc.Handle, target.Handle);
			if (raw_ret != IntPtr.Zero)
				r = new Node (raw_ret);
			else
				r = null;
			return r;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_sources (IntPtr model, IntPtr arc, IntPtr target);

		public Iterator GetSources (Node arc, Node target)
		{
			IntPtr raw_ret = librdf_model_get_sources (model, arc.Handle, target.Handle);
			Iterator it = new Iterator (raw_ret);
			return it;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_arc (IntPtr model, IntPtr source, IntPtr target);

		public Node GetPredicate (Node source, Node target)
		{
			Node r;
			IntPtr raw_ret = librdf_model_get_arc (model, source.Handle, target.Handle);
			if (raw_ret != IntPtr.Zero)
				r = new Node (raw_ret);
			else
				r = null;
			return r;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_target (IntPtr model, IntPtr source, IntPtr arc);

		public Node GetTarget (Node source, Node arc)
		{
			IntPtr raw_ret = librdf_model_get_target (model, source.Handle, arc.Handle);
			Node r = null;
			if (raw_ret != IntPtr.Zero)
				r = new Node (raw_ret);
			else
				throw new Exception ("Model.GetTarget failed");
			return r;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_targets (IntPtr model, IntPtr source, IntPtr arc);

		public Iterator GetTargets (Node source, Node arc)
		{
			IntPtr raw_ret = librdf_model_get_targets (model, source.Handle, arc.Handle);
			Iterator it = null;
			if (raw_ret != IntPtr.Zero)
				it = new Iterator (raw_ret);
			return it;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_arcs (IntPtr model, IntPtr source, IntPtr target);

		public Iterator GetPredicates (Node source, Node target)
		{
			IntPtr raw_ret = librdf_model_get_arcs (model, source.Handle, target.Handle);
			Iterator it = new Iterator (raw_ret);
			return it;
		}

		[DllImport ("librdf")]
		static extern int librdf_model_contains_statement (IntPtr model, IntPtr statement);

		public bool Contains (Statement stm)
		{
			int r = librdf_model_contains_statement (model, stm.Handle);
			return (r != 0);
		}

		[DllImport ("librdf")]
		static extern int librdf_model_remove_statement (IntPtr model, IntPtr statement);

		public int Remove (Statement statement)
		{
			return librdf_model_remove_statement (model, statement.Handle);
		}

		[DllImport ("librdf")]
		static extern int librdf_model_context_remove_statement (IntPtr model, IntPtr context, IntPtr statement);

		public int Remove (Statement stm, Node context)
		{
			return librdf_model_context_remove_statement (model, context.Handle, stm.Handle);
		}

		[DllImport ("librdf")]
		static extern int librdf_model_size (IntPtr model);

		public int Size {
			get { return librdf_model_size (model); }
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_as_stream (IntPtr model);

		public Stream ToStream ()
		{
			IntPtr raw_ret = librdf_model_as_stream (model);
			Stream stream = new Stream (raw_ret);
			return stream;
		}

		[DllImport ("librdf")]
		static extern int librdf_model_add_statements (IntPtr model, IntPtr statement_stream);

		public int AddStatements (Stream stream)
		{
			return librdf_model_add_statements (model, stream.Handle);
		}

		[DllImport ("librdf")]
		static extern void librdf_model_sync (IntPtr model);

		public void Sync ()
		{
			librdf_model_sync (model);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_contexts (IntPtr model);

		public Iterator GetContexts ()
		{
			IntPtr raw_ret = librdf_model_get_contexts (model);
			Iterator it = new Iterator (raw_ret);
			return it;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_get_feature (IntPtr model, IntPtr uri);

		public Node GetFeature (Uri uri)
		{
			IntPtr raw_ret = librdf_model_get_feature (model, uri.Handle);
			Node r = null;
			if (raw_ret != IntPtr.Zero)
				r = new Node (raw_ret);
			else
				throw new Exception ("Model.GetFeature failed");
			return r;
		}

		[DllImport ("librdf")]
		static extern void librdf_model_set_feature (IntPtr model, IntPtr uri, IntPtr value);

		public void SetFeature (Uri uri, Node value)
		{
			librdf_model_set_feature (model, uri.Handle, value.Handle);
		}

		[DllImport ("librdf")]
		static extern void librdf_model_load (IntPtr model, IntPtr uri, IntPtr mime_type, IntPtr type_uri);

		public void Load (Uri uri, String mime_type, Uri type_uri)
		{
			IntPtr imime = Marshal.StringToHGlobalAuto (mime_type.ToString());
			librdf_model_load (model, uri.Handle, imime, type_uri.Handle);
                        Marshal.FreeHGlobal (imime);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_model_execute (IntPtr model, IntPtr query);

		public QueryResults Execute (Query query)
		{
			IntPtr raw_ret = librdf_model_execute (model, query.Handle);
			QueryResults qr = new QueryResults (query, raw_ret);
			return qr;
		}

	}
}
