with RDF.Auxiliary.Handled_Record;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Namespace_Stack;
with RDF.Raptor.IOStream;

package RDF.Raptor.Term is

   type Term_Record is private;

   type Term_Record_Access is access Term_Record;

   package Term_Handled_Record is new RDF.Auxiliary.Handled_Record(Term_Record, Term_Record_Access);

   type Term_Type_Without_Finalize is new Term_Handled_Record.Base_Object with null record;

   subtype Term_Handle is Term_Handled_Record.Access_Type;

   type Term_Literal_Value is private;

   type Term_Blank_Value is private;

   type Term_Kind is (Unknown,
                      URI,
                      Literal,
                      Blank)
      with Convention => C;
   for Term_Kind use (Unknown => 0,
                      URI     => 1,
                      Literal => 2,
                      -- unused type 3
                      Blank   => 4);

   not overriding function Get_World (Term: Term_Type_Without_Finalize) return RDF.Raptor.World.World_Type_Without_Finalize;

   not overriding function Get_Kind (Term: Term_Type_Without_Finalize) return Term_Kind;

   not overriding function Get_URI (Term: Term_Type_Without_Finalize) return URI_Type_Without_Finalize
      with Pre => Get_Kind(Term) = URI;

   not overriding function Get_Literal (Term: Term_Type_Without_Finalize) return Term_Literal_Value
      with Pre => Get_Kind(Term) = Literal;

   not overriding function Get_Blank (Term: Term_Type_Without_Finalize) return Term_Blank_Value
      with Pre => Get_Kind(Term) = Blank;

   not overriding function Value (Literal: Term_Literal_Value) return String;

   -- The returned URI may be null.
   not overriding function Datatype (Literal: Term_Literal_Value) return RDF.Raptor.URI.URI_Type_Without_Finalize;

   -- Return the language tag or empty string if there are none
   not overriding function Language (Literal: Term_Literal_Value) return String;

   not overriding function Value (Blank: Term_Blank_Value) return String;

   not overriding function To_String (Term: Term_Type_Without_Finalize) return String;

   not overriding function Compare (Left, Right: Term_Type_Without_Finalize) return RDF.Auxiliary.Comparison_Result;

   not overriding function Equals (Left, Right: Term_Type_Without_Finalize) return Boolean;

   overriding function "=" (Left, Right: Term_Type_Without_Finalize) return Boolean renames Equals;

   not overriding function To_Turtle_String(Term: Term_Type_Without_Finalize;
                                            Stack: RDF.Raptor.Namespace_Stack.Namespace_Stack_Type'Class;
                                            Base_URI: URI_Type'Class)
                                            return String;

   not overriding procedure Turtle_Write (Stream: RDF.Raptor.IOStream.Stream_Type_Without_Finalize'Class;
                                          Term: Term_Type_Without_Finalize;
                                          Stack: RDF.Raptor.Namespace_Stack.Namespace_Stack_Type'Class;
                                          Base_URI: URI_Type'Class);

   not overriding function Copy (Object: Term_Type_Without_Finalize'Class) return Term_Type_Without_Finalize;

   type Term_Type is new Term_Type_Without_Finalize with null record;

   not overriding function From_Blank (World: World_Type_Without_Finalize'Class) return Term_Type;
   not overriding function From_Blank (World: World_Type_Without_Finalize'Class; ID: String) return Term_Type;
   not overriding function From_Blank (World: World_Type_Without_Finalize'Class; ID: RDF.Auxiliary.String_Holders.Holder) return Term_Type;

   not overriding function From_Literal (World   : World_Type_Without_Finalize'Class;
                                         Literal : RDF.Auxiliary.String_Holders.Holder;
                                         Datatype: RDF.Raptor.URI.URI_Type_Without_Finalize'Class;
                                         Language: RDF.Auxiliary.String_Holders.Holder)
                                         return Term_Type;

   not overriding function From_URI_String (World: World_Type_Without_Finalize'Class; URI: URI_String) return Term_Type;

   not overriding function From_URI (World: World_Type_Without_Finalize'Class; URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class)
                                     return Term_Type;

   not overriding function From_String (World: World_Type_Without_Finalize'Class; Value: String) return Term_Type;

   overriding procedure Adjust (Object: in out Term_Type);

   overriding procedure Finalize_Handle (Object: Term_Type; Handle: Term_Handle);

private

   type Term_Literal_Value is
      record
         str: chars_ptr;
         Len: unsigned;
         Datatype: RDF.Raptor.URI.Handle_Type;
         Language: chars_ptr;
         Language_Len: unsigned;
      end record
      with Convention => C;

   type Term_Blank_Value is
      record
         str: chars_ptr;
         Len: unsigned;
      end record
      with Convention => C;

   type Term_Value (Kind: Term_Kind := Term_Kind'First) is
      record
         case Kind is
            when Unknown => null;
            when URI     => URI: RDF.Raptor.URI.Handle_Type;
            when Literal => Literal: Term_Literal_Value;
            when Blank   => Blank: Term_Blank_Value;
         end case;
      end record
      with Unchecked_Union, Convention => C;

   type Term_Record is
      record
         World: RDF.Raptor.World.Handle_Type;
         Usage: Interfaces.C.int; -- intentionally not accessible from our Ada bindings
         Kind: Term_Kind;
         Value: Term_Value;
      end record
      with Convention => C;

end RDF.Raptor.Term;
