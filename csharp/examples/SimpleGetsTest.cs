//
// SimpleGetsTest.cs: NUnit tests for redland-sharp. 
//		  Based on redland/python/redlandtest.py
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren
//

using NUnit.Framework;
using System;
using Redland;

namespace Redland {

	[TestFixture]
	public class SimpleGetsTest {
		
		MemoryStorage storage;
		Model model;
		Node node1, node2, node3;

		[SetUp]
		public void GetReady ()
		{
			storage = new MemoryStorage ();
			model = new Model (storage);
			node1 = new Node (new Redland.Uri ("http://node1/"));
			node2 = new Node (new Redland.Uri ("http://node2/"));
			node3 = new Node (new Redland.Uri ("http://node3/"));
			model.AddStatement (new Statement (node1, node2, node3));
		}

		[Test]
		public void SimpleGets ()
		{
			Node source = model.GetSource (node2, node3);
			NUnit.Framework.Assert.IsTrue (source == node1, "Source should be node1");
			
			// Node target = model.GetTarget (node1, node2);
			// NUnit.Framework.Assert.IsTrue (target == node3, "Target should be node3");

			// Node pred = model.GetPredicate (node1, node3);
			// NUnit.Framework.Assert.IsTrue (pred == node2, "Predicate should be node2");			
		}
	}
}
