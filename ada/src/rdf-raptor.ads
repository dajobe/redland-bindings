package RDF.Raptor is
   pragma Pure;

   type Domain_Type is (None_Domain,
                        IOStream_Domain,
                        Namespace_Domain,
                        Parser_Domain,
                        Qname_Domain,
                        Sax2_Domain,
                        Serializer_Domain,
                        Term_Domain,
                        Turtle_Writer_Domain,
                        URI_Domain,
                        World_Domain,
                        WWW_Domain,
                        XML_Writer_Domain)
     with Convention => C;
   for Domain_Type use (None_Domain => 0,
                        IOStream_Domain => 1,
                        Namespace_Domain => 2,
                        Parser_Domain => 3,
                        Qname_Domain => 4,
                        Sax2_Domain => 5,
                        Serializer_Domain => 6,
                        Term_Domain => 7,
                        Turtle_Writer_Domain => 8,
                        URI_Domain => 9,
                        World_Domain => 10,
                        WWW_Domain => 11,
                        XML_Writer_Domain => 12);
   function Last return Domain_Type renames XML_Writer_Domain;

end RDF.Raptor;
