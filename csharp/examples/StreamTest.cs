//
// StreamTest.cs:
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren 
//

using System.Collections;
using NUnit.Framework;
using Rdf;

namespace Redland {

	[TestFixture]
	public class StreamTest : Assertion {

		MemoryStorage storage;
		Model model;
		Parser parser;

		[SetUp]
		public void GetReady ()
		{
			storage = new MemoryStorage ("contexts='yes'");
			model = new Model (storage);
			parser = new Parser ();
		}

		[Test]
		public void ParserIntoModel () 
		{
			string a = "file:dc.rdf";
			parser.ParseIntoModel (model, a);
			NUnit.Framework.Assert.IsTrue (model.Size == 3);
		}

		[Test]
		public void IterateModel ()
		{
			int i = 0;
			string a = "file:dc.rdf";
			parser.ParseIntoModel (model, a);			
			Stream model_iter = model.ToStream ();

			while (!model_iter.End) {
				i++;
				model_iter.MoveNext ();
			}
			NUnit.Framework.Assert.IsTrue (i == 3, "dc.rdf should have 3 statements");
		}

		[Test]
		public void ParseAsStream ()
		{
			string a = "file:dc.rdf";
			Stream stream = parser.ParseAsStream (a);

			while (!stream.End) {
				model.AddStatement ((Statement) stream.Current);
				stream.MoveNext ();
			}
			NUnit.Framework.Assert.IsTrue (model.Size == 3, "dc.rdf should have 3 statements");
		}

		[Test]
		public void FindStatements1 ()
		{
			int i = 0;
			parser.ParseIntoModel (model, "file:dc.rdf");
			Statement stm = new Statement (new Node (new Uri ("http://purl.org/net/dajobe/")), null, null);
			Stream found_stms = model.FindStatements (stm);

			while (!found_stms.End) {
				i++;
				found_stms.MoveNext ();
			}
			NUnit.Framework.Assert.IsTrue (i == 3, "Should have found_stms 3 statements");
		}

		[Test]
		public void Contains ()
		{
			int i = 0;
			parser.ParseIntoModel (model, "file:dc.rdf");
			Statement stm = new Statement (new Node (new Uri ("http://purl.org/net/dajobe/")),
						       new Node (new Uri ("http://purl.org/dc/elements/1.1/creator")),
						       new Node ("Dave Beckett"));
			Stream found_stms = model.FindStatements (stm);
		       			
			while (!found_stms.End) {
				i++;
				found_stms.MoveNext ();
			}
			NUnit.Framework.Assert.IsTrue (i == 1, "Should have found_stmts 1 statement");
		}

		[Test]
		public void FindStatementsWithContext ()
		{
			int i = 0;
			string a = "file:dc.rdf";
			Node intended_context = new Node (new Uri ("http://example.org/"));
			Stream stream = parser.ParseAsStream (a);

			while (!stream.End) {
				model.AddStatement ((Statement) stream.Current, intended_context);
				stream.MoveNext ();
			}

			Node subject = new Node (new Uri ("http://purl.org/net/dajobe/"));
			Statement stm = new Statement (subject, null, null);

			// FIXME: search with context			
		}

		[Test]
		public void ParserStringAsStream ()
		{
			int rc;
			string content = "\"\"<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\"><rdf:Description rdf:about=\"http://www.redland.opensource.ac.uk/\"><dc:title>Redland RDF</dc:title></rdf:Description></rdf:RDF>\"\"";

			Uri base_uri = new Uri ("http://example.org/base#");
			Stream s = parser.ParseStringAsStream (content, base_uri);
			NUnit.Framework.Assert.IsTrue (s != null, "ParseStringAsStream should have succeeded");
			rc = model.AddStatements (s);
			NUnit.Framework.Assert.IsTrue (rc == 0, "AddStatements should have succeeded");
		}
	}
}
