//
// example3.cs: C#'s port of redland's redland/example3.c
//

using Rdf;
using System;

public class Test {

	public static void Main ()
	{
		Storage storage = new Storage ("hashes", "test", "hash-type='bdb',dir='.'");
		Model model = new Model (storage);

		Node n1 = new Node ("http://purl.org/net/dajobe/");
		Node n2 = new Node ("http://purl.org/dc/elements/1.1/creator");
		Node n3 = new Node ("Dave Beckett", null, 0);
		
		Statement stm = new Statement (n1, n2, n3);
		model.AddStatement (stm);

		IntPtr out = Util.fopen ("example3.out", "w+");
		model.Print (out);
		Util.fclose(out);
	}
}
