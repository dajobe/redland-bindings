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

		IntPtr parser = IntPtr.Zero;

		bool disposed = false;

		public IntPtr Handle {
			get { return parser; }
		}

		public Parser ()
			: this (Redland.World, "rdfxml", "application/rdf+xml", new Uri ("http://www.w3.org/TR/rdf-testcases/#ntriples"))
		{
		}

		public Parser (string name)
			: this (Redland.World, name, "application/rdf+xml", new Uri ("http://www.w3.org/TR/rdf-testcases/#ntriples"))
		{
		}

		public Parser (string name, string mime_type, Uri uri)
			: this (Redland.World, name, mime_type, uri)
		{
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_parser (IntPtr world, IntPtr name, IntPtr mime_type, IntPtr uri);

		private Parser (World  world, string name, string mime_type, Uri uri)
		{
			IntPtr iname = Marshal.StringToHGlobalAuto (name);
			IntPtr imime_type = Marshal.StringToHGlobalAuto (mime_type);
			if (uri == (Uri) null)
				parser = librdf_new_parser (world.Handle, iname, imime_type, IntPtr.Zero);
			else
				parser = librdf_new_parser (world.Handle, iname, imime_type, uri.Handle);

			Marshal.FreeHGlobal (iname);
			Marshal.FreeHGlobal (imime_type);
		}


		[DllImport ("librdf")]
		static extern void librdf_free_parser (IntPtr parser);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// if disposing is true, then dispose of
				// managed resources

				if (parser != IntPtr.Zero) {
					librdf_free_parser (parser);
					parser = IntPtr.Zero;
				}

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
		static extern int librdf_parser_parse_string_into_model (IntPtr parser, IntPtr s, IntPtr base_uri, IntPtr model);

		public int ParseStringIntoModel (string s, Uri base_uri, Model model)
		{
			IntPtr istr = Marshal.StringToHGlobalAuto (s);
			Redland.World.Enter ();
			int rc = librdf_parser_parse_string_into_model (Handle, istr, base_uri.Handle, model.Handle);
			Marshal.FreeHGlobal (istr);
			LogMessage [] errs = Redland.World.Messages;
			Redland.World.Exit ();
			if (errs.Length > 0)
				throw new ParseError ("Parsing error", errs);
			return rc;
		}

		[DllImport ("librdf")]
		static extern int librdf_parser_parse_into_model (IntPtr parser, IntPtr uri, IntPtr base_uri, IntPtr model);

		public int ParseIntoModel (Uri uri, Uri base_uri, Model model)
		{
			Redland.World.Enter ();
			int rc = librdf_parser_parse_into_model (parser, uri.Handle, base_uri.Handle, model.Handle);
			LogMessage [] errs = Redland.World.Messages;
			Redland.World.Exit ();
			if (errs.Length > 0)
				throw new ParseError ("Parsing error", errs);
			// FIXME: can we remove rc now we throw exceptions?
			return rc;
		}

		public int ParseIntoModel (Model model, string uri)
		{			
			return ParseIntoModel (new Redland.Uri (uri), new Redland.Uri (uri), model);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_parser_parse_as_stream (IntPtr parser, IntPtr uri, IntPtr base_uri);

		public Stream ParseAsStream (string uri)
		{
			Uri tmp_uri = new Redland.Uri (uri);
			Redland.World.Enter ();
			IntPtr raw_stream = librdf_parser_parse_as_stream (parser, tmp_uri.Handle, IntPtr.Zero);
			// FIXME: throw exception if raw_stream is zero ?
			LogMessage [] errs = Redland.World.Messages;
			Redland.World.Exit ();
			if (errs.Length > 0)
				throw new ParseError ("Parsing error", errs);
			Stream stream = new Stream (raw_stream);
			return stream;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_parser_parse_string_as_stream (IntPtr parser, IntPtr s, IntPtr base_uri);

		public Stream ParseStringAsStream (string s, Uri base_uri)
		{
			// Console.WriteLine ("Parsing string '{0}' URI {1}", s, base_uri.ToString());

			IntPtr istr = Marshal.StringToHGlobalAuto (s);
			Redland.World.Enter ();
			IntPtr raw_ret = librdf_parser_parse_string_as_stream (parser, istr, base_uri.Handle);
			// FIXME: throw exception if raw_ret is zero ? currently
			// we return null, see below.
			Stream stream;
			Marshal.FreeHGlobal (istr);
			LogMessage [] errs = Redland.World.Messages;
			Redland.World.Exit ();

			if (errs.Length > 0)
				throw new ParseError ("Parsing error", errs);

			if (raw_ret == IntPtr.Zero)
				return null;
			else
				stream = new Stream (raw_ret);
			return stream;
		}
	}
}
