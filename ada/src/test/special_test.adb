-- FIXED: Calling this (compiled with GANT 4.9) as
--   ltrace -n4 -llibraptor2.so.0 ./obj/test/check/special_test 2>&1| egrep ^[a-z]
-- shows that there are more raptor_new_uri_from_counted_string() + raptor_uri_copy()
-- calls than raptor_free_uri() calls.
-- This means memory leak.

with Ada.Finalization;
with Ada.Text_IO;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.World;

procedure Special_Test is

   World: RDF.Raptor.World.World_Type;

   function F return RDF.Raptor.URI.URI_Type is
   begin
      return From_String(World, "http://example.org");
   end;

   X: RDF.Raptor.URI.URI_Type := F;

begin
   null;
end;
