with Interfaces.C;

package RDF.Raptor is

   type Domain_Type is (None,
                        Iostream_Domain,
                        Namespace,
                        Parser,
                        Qname,
                        Sax2,
                        Serializer,
                        Term_Domain,
                        Turtle_Writer,
                        URI_Domain,
                        World_Domain,
                        WWW,
                        XML_Writer)
      with Convention => C;
   for Domain_Type use (None => 0,
                        Iostream_Domain => 1,
                        Namespace => 2,
                        Parser => 3,
                        Qname => 4,
                        Sax2 => 5,
                        Serializer => 6,
                        Term_Domain => 7,
                        Turtle_Writer => 8,
                        URI_Domain => 9,
                        World_Domain => 10,
                        WWW => 11,
                        XML_Writer => 12);
   function Last return Domain_Type renames XML_Writer;

end RDF.Raptor;
