//
// ModelGeneralTest.cs:
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
	public class ModelGeneralTest : Assertion {

		HashStorage storage;
		Model model;

		[SetUp]
		public void GetReady ()
		{
			storage = new HashStorage ("test", "hash-type='bdb'");
			model = new Model (storage);
		}

		[Test]
		public void Sync ()
		{
			model.Sync ();
		}
	}
}
