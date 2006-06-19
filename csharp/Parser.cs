//
// Parser.cs: Redland Syntax Parser class.
//
// $Id$
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren

using System;
using System.Runtime.InteropServices;

namespace Redland {

	public class ParseError : RedlandError {
		public ParseError (string msg, LogMessage [] errs) :
			base (msg, errs) { }
	}

	public class Parser : IWrapper, IDisposable {

		private HandleRef handle;

		private bool disposed = false;
		private World world = Redland.World.AddReference ();

		public HandleRef Handle {
			get { return handle; }
		}

		public Parser ()
			: this ("rdfxml", null, null)
		{
		}

		public Parser (string name)
			: this (name, null, null)
		{
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_parser (HandleRef world, IntPtr name, IntPtr mime_type, IntPtr uri);

		public Parser (string name, string mime_type, Uri uri)
		{
			IntPtr iname = Util.StringToHGlobalUTF8 (name);
			IntPtr imime_type = Util.StringToHGlobalUTF8 (mime_type);
			IntPtr parser;
			if (uri == (Uri) null)
				parser = librdf_new_parser (world.Handle, iname, imime_type, IntPtr.Zero);
			else
				parser = librdf_new_parser (world.Handle, iname, imime_type, uri.Handle.Handle);
			handle = new HandleRef (this, parser);
			Marshal.FreeHGlobal (iname);
			Marshal.FreeHGlobal (imime_type);
		}


		[DllImport ("librdf")]
		static extern void librdf_free_parser (HandleRef parser);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing is true, then dispose of
				// managed resources

				if (handle.Handle != IntPtr.Zero) {
					librdf_free_parser (handle);
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

		~Parser ()
		{
			Dispose (false);
		}


		[DllImport ("librdf")]
		static extern int librdf_parser_parse_string_into_model (HandleRef parser, IntPtr s, HandleRef base_uri, HandleRef model);

		public int ParseStringIntoModel (string s, Uri base_uri, Model model)
		{
			IntPtr istr = Util.StringToHGlobalUTF8 (s);
			world.Enter ();
			int rc = librdf_parser_parse_string_into_model (handle, istr, base_uri.Handle, model.Handle);
			Marshal.FreeHGlobal (istr);
			LogMessage [] errs = world.Messages;
			world.Exit ();
			if (errs.Length > 0)
				throw new ParseError ("Parsing error", errs);
			return rc;
		}

		[DllImport ("librdf")]
		static extern int librdf_parser_parse_into_model (HandleRef parser, HandleRef uri, HandleRef base_uri, HandleRef model);

		public int ParseIntoModel (Uri uri, Uri base_uri, Model model)
		{
			world.Enter ();
			int rc = librdf_parser_parse_into_model (handle, uri.Handle, base_uri.Handle, model.Handle);
			LogMessage [] errs = world.Messages;
			world.Exit ();
			if (errs.Length > 0)
				throw new ParseError ("Parsing error", errs);
			// FIXME: can we remove rc now we throw exceptions?
			return rc;
		}

		public int ParseIntoModel (Model model, string uri)
		{			
			return ParseIntoModel (new Uri (uri), new Uri (uri), model);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_parser_parse_as_stream (HandleRef parser, HandleRef uri, IntPtr base_uri);

		public Stream ParseAsStream (string uri)
		{
			Uri tmp_uri = new Uri (uri);
			world.Enter ();
			IntPtr raw_stream = librdf_parser_parse_as_stream (handle, tmp_uri.Handle, IntPtr.Zero);
			// FIXME: throw exception if raw_stream is zero ?
			LogMessage [] errs = world.Messages;
			world.Exit ();
			if (errs.Length > 0)
				throw new ParseError ("Parsing error", errs);
			Stream stream = new Stream (raw_stream);
			return stream;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_parser_parse_string_as_stream (HandleRef parser, IntPtr s, HandleRef base_uri);

		public Stream ParseStringAsStream (string s, Uri base_uri)
		{
			// Console.WriteLine ("Parsing string '{0}' URI {1}", s, base_uri.ToString());

			IntPtr istr = Util.StringToHGlobalUTF8 (s);

			world.Enter ();
			IntPtr raw_ret = librdf_parser_parse_string_as_stream (handle, istr, base_uri.Handle);
			// FIXME: throw exception if raw_ret is zero ? currently
			// we return null, see below.
			Marshal.FreeHGlobal (istr);
			LogMessage [] errs = world.Messages;
			world.Exit ();

			if (errs.Length > 0)
				throw new ParseError ("Parsing error", errs);

			if (raw_ret == IntPtr.Zero)
				return null;

			return new Stream (raw_ret);
		}
	}
}
