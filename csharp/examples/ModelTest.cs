//
// ModelTest.cs: NUnit tests for redland-sharp. 
//		 Based on redland/python/redlandtest.py
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren
//

using NUnit.Framework;
using System;
using Rdf;

namespace Redland {

	[TestFixture]
	public class ModelTest : Assertion {
		
		Storage storage_no_context;
		Storage storage_context;
		Node context_node;
		Node alien_context_node;
		Model model;

		[SetUp]
		public void GetReady ()
		{
			storage_no_context = new Storage ("hashes", "redland_testcase_nc",
							  "hash-type='bdb',contexts='no',dir='/tmp',new='yes'");
			storage_context = new Storage ("hashes", "redland_testcase_c", 
						       "hash-type='bdb',contexts='yes',dir='/tmp',new='yes'");
			context_node = new Node (new Rdf.Uri ("http://mycontext/"));
			alien_context_node = new Node (new Rdf.Uri ("http://mycontext/alien"));
		}

		private static Statement MakeStatement (int n)
		{
			return new Statement (new Node (new Rdf.Uri ("http://subject/" + n)),
					      new Node (new Rdf.Uri ("http://pred/" + n)),
					      new Node (new Rdf.Uri ("http://object/" + n)));
		}

		[Test]
		public void ListnessNoContext ()
		{			
			int i;			
			Statement stm, stm2;

			model = new Model (storage_no_context);		       
			
			for (i = 0; i < 4; i++) { 
				stm = MakeStatement (i);
				model.AddStatement (stm);
			}

			for (i = 0; i < 4; i++) {
				stm = MakeStatement (i);
				NUnit.Framework.Assert.IsTrue (model.Contains (stm), 
							       "statement " + stm.ToString () +  " should be in model.");
			}

			stm2 = MakeStatement (2);
			model.Remove (stm2);
			NUnit.Framework.Assert.IsTrue (!model.Contains (stm2), 
						       "Remove failed; statement " + stm2.ToString () + " should not be in model.");
		}

		[Test]
		public void ListnessContext ()
		{
			int i;			
			Statement stm;
			model = new Model (storage_context);

			for (i = 0; i < 4; i++) {
				stm = MakeStatement (i);
				model.AddStatement (stm, context_node);
			}

			for (i = 0; i < 4; i++) {
				stm = MakeStatement (i);
				// FIXME: how do I search a statement with a specified context?
				// NUnit.Framework.Assert.IsTrue (model.Contains (stm, context_node));
				// Nunit.Framework.Assert.IsTrue (!model.Contains (stm, alien_context_node));
			}
			
			Statement stm2 = MakeStatement (2);
			model.Remove (stm2, context_node);
			// FIXME: how do I search a statement with a specified context?
			// Nunit.Framework.Assert.IsTrue (!model.Contains (stm2, context_node));
		}
	}
}
