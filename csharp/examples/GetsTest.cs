//
// GetsTest.cs: NUnit test for redland-sharp.
//		Based on redland/python/redlandtest.py
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren
//

using NUnit.Framework;
using Rdf;

namespace Redland {

	[TestFixture]
	public class GetsTest : Assertion {
	
		MemoryStorage storage, storage_c;
		Model model, model_c;
		Node node1, node2, node3, node4, node5, node6, nodeb, context_node;

		[SetUp]
		public void GetReady ()
		{
			storage = new MemoryStorage ();
			model = new Model (storage);
			node1 = new Node (new Rdf.Uri ("http://node1/"));
			node2 = new Node (new Rdf.Uri ("http://node2/"));
			node3 = new Node (new Rdf.Uri ("http://node3/"));
			node4 = new Node (new Rdf.Uri ("http://node4/"));
			node5 = new Node (new Rdf.Uri ("http://node5/"));
			node6 = new Node (new Rdf.Uri ("http://node6/"));
			nodeb = new Node ();
			model.AddStatement (new Statement (node1, node2, node3));
			model.AddStatement (new Statement (node1, node2, node4));
			model.AddStatement (new Statement (node6, node2, node4));
			model.AddStatement (new Statement (node5, node3, node6));
			model.AddStatement (new Statement (node5, node1, node6));

			storage_c = new MemoryStorage ("contexts='yes'");
			model_c = new Model (storage_c);
			context_node = new Node (new Rdf.Uri ("http://mycontext/"));

			model_c.AddStatement (new Statement (node1, node2, node3), context_node);
			model_c.AddStatement (new Statement (node1, node2, node4), context_node);
			model_c.AddStatement (new Statement (node6, node2, node4), context_node);
			model_c.AddStatement (new Statement (node5, node3, node6), context_node);
			model_c.AddStatement (new Statement (node5, node1, node6), context_node);
			
		}

		[Test]
		public void Gets ()
		{
			Iterator sources = model.GetSources (node2, node4);
			// FIXME:
			// NUnit.Framework.Assert.IsTrue (sources.Contains (node1) && sources.Contains (node6));
			Iterator targets = model.GetTargets (node1, node2);
			Iterator predicates = model.GetPredicates (node5, node5);
		}

		[Test]
		public void GetsContext ()
		{
			// FIXME: search with context.
		}
	}
}
