//
// tester.cs: play with new Redland's C# objects. Based on Redland's redland/example/example1.c
//

using Rdf;
using System;
using System.Runtime.InteropServices;

public class Test {
	
	static string rdfxml_content =
	"<?xml version=\"1.0\"?> <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\"> <rdf:Description rdf:about=\"http://purl.org/net/dajobe/\"> <dc:title>Dave Beckett's Home Page </dc:title> <dc:creator>Dave Beckett </dc:creator> <dc:description> The generic homepage of Dave Beckett. </dc:description> </rdf:Description></rdf:RDF>";

	[DllImport ("libc", EntryPoint="fopen")]
	public static extern IntPtr fopen (string path, string mode);

	public static void Main ()
	{
		Rdf.World world = new Rdf.World ();
		world.Open ();

		Rdf.Uri uri = new Rdf.Uri (world, "http://example.librdf.org/");
		Storage storage = new Storage (world, "memory", "test", null);
		Model model = new Model (world, storage, null);

		Parser parser = new Parser (world, "raptor", null, null);
		parser.ParseStringIntoModel (rdfxml_content, uri, model);

		Node subject, predicate, obj;
		subject = new Node (world, "http://purl.org/net/dajobe/");
		predicate = new Node (world, "http://purl.org/dc/elements/1.1/title");
		obj = new Node (world, "My Home Page", null, 0);

		Statement stm = new Statement (world, subject, predicate, obj);
		model.AddStatement (stm);

		IntPtr output = fopen ("test-cesar-redland-binding.xml", "w+");
		model.Print (output);

		Statement partial_stm = new Statement (world);
		partial_stm.Subject = subject;
		partial_stm.Predicate = predicate;

		// FIXME: return types not handled at all.
		Stream stream = model.FindStatements (partial_stm);
		Iterator iterator = model.GetTargets (subject, predicate);
	}
}
