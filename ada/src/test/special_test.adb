-- FIXME: Calling this (compiled with GANT 4.9) as
--   ltrace -n4 -llibraptor2.so.0 ./obj/test/check/special_test 2>&1| egrep ^[a-z] | grep _uri
-- shows that there are less raptor_new_uri_from_counted_string() + raptor_uri_copy()
-- calls than raptor_free_uri() calls.
-- This means that some object is deallocated twice.

with Ada.Text_IO;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Term; use RDF.Raptor.Term;
with RDF.Raptor.World;
with RDF.Auxilary;

procedure Special_Test is

   World: RDF.Raptor.World.World_Type;

   Term_1: Term_Type := From_Literal (World,
                                      RDF.Auxilary.String_Holders.To_Holder("QWE"),
                                      From_String(World, "http://example.org"), -- datatype
                                      RDF.Auxilary.String_Holders.Empty_Holder -- language
                                     );

   Str: String := To_String(Datatype(Get_Literal(Term_1)));

begin
   null;
end;
