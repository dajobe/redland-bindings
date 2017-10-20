with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;
limited with RDF.Raptor.Namespace_Stack;

package RDF.Raptor.Namespace is

   package Namespace_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Namespace_Handle is Namespace_Handled_Record.Access_Type;

   type Namespace_Type_Without_Finalize is new Namespace_Handled_Record.Base_Object with null record;

   overriding procedure Finalize_Handle (Object: Namespace_Type_Without_Finalize; Handle: Namespace_Handle);

   not overriding function Get_URI (NS: Namespace_Type_Without_Finalize) return URI_Type_Without_Finalize;

   not overriding function Get_Prefix (NS: Namespace_Type_Without_Finalize) return String;

   not overriding procedure Write (NS: Namespace_Type_Without_Finalize; Stream: Base_IOStream_Type'Class);

   not overriding function Format_As_XML (NS: Namespace_Type_Without_Finalize) return String;

   package Finalizer is new Namespace_Handled_Record.With_Finalization(Namespace_Type_Without_Finalize);

   type Namespace_Type is new Finalizer.Derived with null record;

   not overriding function New_Namespace (Stack: RDF.Raptor.Namespace_Stack.Namespace_Stack_Type_Without_Finalize'Class;
                                          Prefix: String;
                                          NS: String;
                                          Depth: Natural)
                                          return Namespace_Type;

   not overriding function From_URI (Stack: RDF.Raptor.Namespace_Stack.Namespace_Stack_Type_Without_Finalize'Class;
                                     Prefix: String;
                                     URI: URI_Type_Without_Finalize'Class;
                                     Depth: Integer)
                                     return Namespace_Type;

   type Prefix_And_URI(<>) is private;

   function Get_Prefix (Object: Prefix_And_URI) return String;
   function Get_URI (Object: Prefix_And_URI) return URI_String;

   -- See also Extract_Prefix and Extract_URI
   function String_Parse (NS: String) return Prefix_And_URI;

   function Extract_Prefix (NS: String) return String;
   function Extract_URI    (NS: String) return URI_String;

private

   type Prefix_And_URI (Prefix_Length, URI_Length: Natural) is
      record
         Prefix: String(1..Prefix_Length);
         URI: URI_String(1..URI_Length);
      end record;

end RDF.Raptor.Namespace;
