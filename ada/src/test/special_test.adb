-- FIXED: Calling this (compiled with GANT 4.9) as
--   ltrace -n4 -llibraptor2.so.0 ./obj/test/check/special_test 2>&1| egrep ^[a-z] | grep _uri
-- shows that there are less raptor_new_uri_from_counted_string() + raptor_uri_copy()
-- calls than raptor_free_uri() calls.
-- This means that some object is deallocated twice.

with Ada.Text_IO;
with RDF.Raptor.Options; use RDF.Raptor.Options;

procedure Special_Test is
   Min: constant := Raptor_Option'Base'Pos(Raptor_Option'Base'First);
   Max: constant := Raptor_Option'Base'Pos(Raptor_Option'Base'Last);
begin
   Ada.Text_IO.Put_Line(Integer'Image(Min) & " - " & Integer'Image(Max));
end;
