//
// Parser.cs: Redland Syntax Parser class.
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren

using System;
using System.Runtime.InteropServices;

namespace Rdf {

	public class Parser {

		IntPtr parser;

		public IntPtr Handle {
			get { return parser; }
		}

		public Parser ()
			: this (Redland.World, "raptor", "application/rdf+xml", new Uri ("http://www.w3.org/TR/rdf-testcases/#ntriples"))
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
		static extern IntPtr librdf_new_parser (IntPtr world, string name, string mime_type, IntPtr uri);

		private Parser (World  world, string name, string mime_type, Uri uri)
		{
			if (world == null)
				if (uri == null)
					parser = librdf_new_parser (IntPtr.Zero, name, mime_type, IntPtr.Zero);
				else
					parser = librdf_new_parser (IntPtr.Zero, name, mime_type, uri.Handle);
			else if (uri == null)
				parser = librdf_new_parser (world.Handle, name, mime_type, IntPtr.Zero);
			else
				parser = librdf_new_parser (world.Handle, name, mime_type, uri.Handle);
		}



		[DllImport ("librdf")]
		static extern int librdf_parser_parse_string_into_model (IntPtr parser, string s, IntPtr base_uri, IntPtr model);
		public int ParseStringIntoModel (string s, Uri base_uri, Model model)
		{
			return librdf_parser_parse_string_into_model (Handle, s, base_uri.Handle, model.Handle);
		}

		[DllImport ("librdf")]
		static extern int librdf_parser_parse_into_model (IntPtr parser, IntPtr uri, IntPtr base_uri, IntPtr model);

		public int ParseIntoModel (Uri uri, Uri base_uri, Model model)
		{
			return librdf_parser_parse_into_model (parser, uri.Handle, base_uri.Handle, model.Handle);
		}

		public int ParseIntoModel (Model model, string uri)
		{			
			return ParseIntoModel (new Rdf.Uri (uri), new Rdf.Uri (uri), model);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_parser_parse_as_stream (IntPtr parser, IntPtr uri, IntPtr base_uri);

		public Stream ParseAsStream (string uri)
		{
			Uri tmp_uri = new Rdf.Uri (uri);
			IntPtr raw_stream = librdf_parser_parse_as_stream (parser, tmp_uri.Handle, IntPtr.Zero);
			Stream stream = new Stream (raw_stream);
			return stream;
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_parser_parse_string_as_stream (IntPtr parser, string s, IntPtr base_uri);

		public Stream ParseStringAsStream (string s, Uri base_uri)
		{
			IntPtr raw_ret = librdf_parser_parse_string_as_stream (parser, s, base_uri.Handle);
			Stream stream;

			if (raw_ret == IntPtr.Zero)
				return null;
			else
				stream = new Stream (raw_ret);
			return stream;
		}
	}
}
