//
// example3.cs: C#'s port of redland's redland/example3.c
//

using Redland;
using System;

public class Test {

	public static void Main ()
	{
		Storage storage = new Storage ("hashes", "test", "hash-type='bdb',dir='.'");
		Model model = new Model (storage);

		Node n1 = new Node (new Redland.Uri ("http://purl.org/net/dajobe/"));
		Node n2 = new Node (new Redland.Uri ("http://purl.org/dc/elements/1.1/creator"));
		Node n3 = new Node ("Dave Beckett");
		
		Statement stm = new Statement (n1, n2, n3);
		model.AddStatement (stm);

		IntPtr fh = Util.fopen ("example3.out", "w+");
		model.Print (fh);
		Util.fclose (fh);
	}
}
