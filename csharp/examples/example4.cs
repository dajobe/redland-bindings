//
// example4.cs: C#'s port of redland's redland/example4.c
//

using Rdf;
using System;

public class Test {

	public static void Main ()
	{
		Storage storage = new Storage ("hashes", "test", "hash-type='bdb',dir='.'");
		Model model = new Model (storage);

		Parser parser = new Parser ("raptor", "application/rdf+xml", null);
		Rdf.Uri uri = new Rdf.Uri ("file:../../data/dc.rdf");
		parser.ParseIntoModel (uri, uri, model);

		Serializer serializer = new Serializer ("rdfxml", null, null);
		Rdf.Uri base_uri = new Rdf.Uri ("http://example.org/base.rdf");

		serializer.SerializeModel ("example4.xml", base_uri, model);
	}
}
