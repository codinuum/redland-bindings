//
// example1.cs: C# port of Redland's redland/example/example1.c
//

using Rdf;
using System;

public class Test {
	
	static string rdfxml_content =
	"<?xml version=\"1.0\"?> <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\"> <rdf:Description rdf:about=\"http://purl.org/net/dajobe/\"> <dc:title>Dave Beckett's Home Page </dc:title> <dc:creator>Dave Beckett </dc:creator> <dc:description> The generic homepage of Dave Beckett. </dc:description> </rdf:Description></rdf:RDF>";

	public static void Main ()
	{
		Rdf.Uri uri = new Rdf.Uri ("http://example.librdf.org/");
		Storage storage = new Storage ("memory", "test", null);
		Model model = new Model (storage);

		Parser parser = new Parser ("raptor");
		Console.WriteLine ("Parsing URI: {0}", uri.ToString ());
		parser.ParseStringIntoModel (rdfxml_content, uri, model);

		Node subject, predicate, obj;
		subject = new Node ("http://purl.org/net/dajobe/");
		predicate = new Node ("http://purl.org/dc/elements/1.1/title");
		obj = new Node ("My Home Page", null, 0);

		Statement stm = new Statement (subject, predicate, obj);
		model.AddStatement (stm);

		IntPtr output = Util.fopen ("example1.xml", "w+");
		model.Print (output);

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