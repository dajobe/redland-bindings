//
// example1.cs: C# port of Redland's redland/example/example1.c
//

using Redland;
using System;

public class Test {
	
	static string rdfxml_content =
	"<?xml version=\"1.0\"?> <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\"> <rdf:Description rdf:about=\"http://purl.org/net/dajobe/\"> <dc:title>Dave Beckett's Home Page </dc:title> <dc:creator>Dave Beckett </dc:creator> <dc:description> The generic homepage of Dave Beckett. </dc:description> </rdf:Description></rdf:RDF>";

	public static void Main ()
	{
		Redland.Uri uri = new Redland.Uri ("http://example.librdf.org/");
		Storage storage = new Storage ("memory", "test", null);
		Model model = new Model (storage);

		Parser parser = new Parser ("raptor");
		Console.WriteLine ("Parsing URI: {0}", uri.ToString ());
		parser.ParseStringIntoModel (rdfxml_content, uri, model);

		Node subject, predicate, obj;
		subject = new Node (new Redland.Uri("http://purl.org/net/dajobe/"));
		predicate = new Node (new Redland.Uri("http://purl.org/dc/elements/1.1/title"));
		obj = new Node ("My Home Page");

		Statement stm = new Statement (subject, predicate, obj);
		model.AddStatement (stm);

		Serializer serializer = new Serializer ("rdfxml", null, null);
		Redland.Uri base_uri = new Redland.Uri ("http://example.org/base#");
		serializer.SerializeModel ("example1.xml", base_uri, model);

		Statement partial_stm = new Statement ();
		partial_stm.Subject = subject;
		partial_stm.Predicate = predicate;
		
		Stream stream = model.FindStatements (partial_stm);
		int count = 0;
		while (!stream.End) {
			Statement statement = (Statement) stream.Current;
			Console.Write ("Matched statement: ");
			Console.WriteLine (statement.ToString ());
			stream.MoveNext ();
			count++;
		}

		Console.WriteLine ("Got {0} matching statements.", count);

		Iterator iterator = model.GetTargets (subject, predicate);

 		while (!iterator.End) {
			Node target = (Node) iterator.Current;
			Console.Write ("Matched target: ");
			Console.WriteLine (target.ToString ());
			iterator.MoveNext ();
		}
	}
}
