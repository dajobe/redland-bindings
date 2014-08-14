with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxilary; use RDF.Auxilary;
with RDF.Auxilary.C_String_Holders; use RDF.Auxilary.C_String_Holders;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Namespaces_Stacks; use RDF.Raptor.Namespaces_Stacks;

package body RDF.Raptor.Namespaces is

   function C_Raptor_New_Namespace_From_URI (Stack: RDF.Raptor.Namespaces_Stacks.Namespace_Stack_Handle_Type;
                                             Prefix: char_array;
                                             URI: RDF.Raptor.URI.Handle_Type)
                                             return Namespace_Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_namespace_from_uri";

   function From_URI (Stack: RDF.Raptor.Namespaces_Stacks.Namespace_Stack_Type_Without_Finalize'Class;
                      Prefix: String;
                      URI: URI_Type_Without_Finalize'Class;
                      Depth: Integer)
                      return Namespace_Type is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Namespace_From_URI (Get_Handle(Stack), To_C(Prefix, Append_Nul=>True), Get_Handle(URI)) );
   end;

end RDF.Raptor.Namespaces;
