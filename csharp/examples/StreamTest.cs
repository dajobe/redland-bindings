//
// StreamTest.cs:
//
// $Id$
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren 
//

using System.Collections;
using NUnit.Framework;
using Redland;

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
			string a = "file:../../data/dc.rdf";
			parser.ParseIntoModel (model, a);
			NUnit.Framework.Assert.IsTrue (model.Size == 3);
		}

		[Test]
		public void IterateModel ()
		{
			int i = 0;
			string a = "file:../../data/dc.rdf";
			parser.ParseIntoModel (model, a);
			foreach (Statement s in model.ToStream ())
				i++;

			NUnit.Framework.Assert.IsTrue (i == 3, "../../data/dc.rdf should have 3 statements");
		}

		[Test]
		public void ParseAsStream ()
		{
			string a = "file:../../data/dc.rdf";
			foreach (Statement s in parser.ParseAsStream (a)) 
				model.AddStatement (s);

			NUnit.Framework.Assert.IsTrue (model.Size == 3, "../../data/dc.rdf should have 3 statements");
		}

		[Test]
		public void FindStatements1 ()
		{
			int i = 0;
			parser.ParseIntoModel (model, "file:../../data/dc.rdf");
			Statement stm = new Statement (new Node (new Uri ("http://purl.org/net/dajobe/")), null, null);
			foreach (Statement s in model.FindStatements (stm)) 
				i++;

			NUnit.Framework.Assert.IsTrue (i == 3, "Should have found_stms 3 statements");
		}

		[Test]
		public void Contains ()
		{
			bool b=false;
			parser.ParseIntoModel (model, "file:../../data/dc.rdf");
			Statement stm = new Statement (new Node (new Redland.Uri ("http://purl.org/net/dajobe/")),
						       new Node (new Redland.Uri ("http://purl.org/dc/elements/1.1/creator")),
						       new Node ("Dave Beckett"));
			b=model.Contains(stm);
			NUnit.Framework.Assert.IsTrue (b, "Should have found_stmts 1 statement");
		}

		[Test]
		public void FindStatementsWithContext ()
		{
			int i = 0;
			string a = "file:../../data/dc.rdf";
			Node intended_context = new Node (new Uri ("http://example.org/"));
			foreach (Statement s in parser.ParseAsStream (a))
				model.AddStatement (s, intended_context);

			Node subject = new Node (new Uri ("http://purl.org/net/dajobe/"));
			Statement stm = new Statement (subject, null, null);

			// FIXME: search with context			
		}

		[Test]
		public void ParserStringAsStream ()
		{
			int rc;
			string content = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\"><rdf:Description rdf:about=\"http://www.redland.opensource.ac.uk/\"><dc:title>Redland RDF</dc:title></rdf:Description></rdf:RDF>";

			Uri base_uri = new Uri ("http://example.org/base#");
			Stream s = parser.ParseStringAsStream (content, base_uri);
			NUnit.Framework.Assert.IsTrue (s != null, "ParseStringAsStream should have succeeded");
			rc = model.AddStatements (s);
			NUnit.Framework.Assert.IsTrue (rc == 0, "AddStatements should have succeeded");
		}
	}
}
