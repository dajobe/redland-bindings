with RDF.Auxiliary.Handled_Record;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Namespace_Stack;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;

package RDF.Raptor.Term is

   type Term_Record is private;

   type Term_Record_Access is access all Term_Record with Convention=>C;

   package Term_Handled_Record is new RDF.Auxiliary.Handled_Record(Term_Record, Term_Record_Access);

   type Term_Type_Without_Finalize is new Term_Handled_Record.Base_Object with null record;

   subtype URI_Term_Type_Without_Finalize is Term_Type_Without_Finalize
     with Dynamic_Predicate => Is_URI(URI_Term_Type_Without_Finalize);
   subtype Literal_Term_Type_Without_Finalize is Term_Type_Without_Finalize
     with Dynamic_Predicate => Is_Literal(Literal_Term_Type_Without_Finalize);
   subtype Blank_Term_Type_Without_Finalize is Term_Type_Without_Finalize
     with Dynamic_Predicate => Is_Blank(Blank_Term_Type_Without_Finalize);

   subtype Term_Handle is Term_Handled_Record.Access_Type;

   overriding function Adjust_Handle (Object: Term_Type_Without_Finalize; Handle: Term_Handle) return Term_Handle;

   overriding procedure Finalize_Handle (Object: Term_Type_Without_Finalize; Handle: Term_Handle);

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

   not overriding function Get_World (Term: Term_Type_Without_Finalize)
                                      return Raptor_World_Type_Without_Finalize;

   not overriding function Get_Kind (Term: Term_Type_Without_Finalize) return Term_Kind;

   not overriding function Is_URI     (Term: Term_Type_Without_Finalize) return Boolean;
   not overriding function Is_Literal (Term: Term_Type_Without_Finalize) return Boolean;
   not overriding function Is_Blank   (Term: Term_Type_Without_Finalize) return Boolean;

   function Get_URI (Term: URI_Term_Type_Without_Finalize'Class) return URI_Type_Without_Finalize;

   -- Should be private - https://gcc.gnu.org/bugzilla/show_bug.cgi?id=82639
   type Term_Literal_Value is
      record
         str: chars_ptr;
         Len: unsigned;
         Datatype: URI_Handle;
         Language: chars_ptr;
         Language_Len: unsigned;
      end record
     with Convention => C;

   -- Should be private - https://gcc.gnu.org/bugzilla/show_bug.cgi?id=82639
   type Term_Blank_Value is
      record
         str: chars_ptr;
         Len: unsigned;
      end record
     with Convention => C;

   function Get_Literal (Term: Literal_Term_Type_Without_Finalize'Class) return Term_Literal_Value;

   function Get_Blank (Term: Blank_Term_Type_Without_Finalize'Class) return Term_Blank_Value;

   not overriding function Value (Literal: Term_Literal_Value) return String;

   -- The returned URI may be null.
   not overriding function Datatype (Literal: Term_Literal_Value) return URI_Type_Without_Finalize;

   -- Return the language tag or empty string if there are none
   not overriding function Language (Literal: Term_Literal_Value) return String;

   not overriding function Value (Blank: Term_Blank_Value) return String;

   not overriding function To_String (Term: Term_Type_Without_Finalize) return String;

   not overriding function Compare (Left, Right: Term_Type_Without_Finalize)
                                    return RDF.Auxiliary.Comparison_Result;

   not overriding function Equals (Left, Right: Term_Type_Without_Finalize) return Boolean;

   overriding function "=" (Left, Right: Term_Type_Without_Finalize) return Boolean renames Equals;

   not overriding function To_Turtle_String(Term: Term_Type_Without_Finalize;
                                            Stack: RDF.Raptor.Namespace_Stack.Namespace_Stack_Type'Class;
                                            Base_URI: URI_Type'Class)
                                            return String;

   not overriding procedure Turtle_Write (Stream: IOStream_Type_Without_Finalize'Class;
                                          Term: Term_Type_Without_Finalize;
                                          Stack: RDF.Raptor.Namespace_Stack.Namespace_Stack_Type'Class;
                                          Base_URI: URI_Type'Class);

   package Handlers is new Term_Handled_Record.Common_Handlers(Term_Type_Without_Finalize);

   type Term_Type is new Handlers.Base_With_Finalization with null record;

   type Term_Type_User is new Handlers.User_Type with null record;

   not overriding function From_Blank (World: Raptor_World_Type_Without_Finalize'Class)
                                       return Term_Type;
   not overriding function From_Blank (World: Raptor_World_Type_Without_Finalize'Class; ID: String)
                                       return Term_Type;
   not overriding function From_Blank (World: Raptor_World_Type_Without_Finalize'Class; ID: RDF.Auxiliary.String_Holders.Holder)
                                       return Term_Type;

   not overriding function From_Literal (World   : Raptor_World_Type_Without_Finalize'Class;
                                         Literal : RDF.Auxiliary.String_Holders.Holder;
                                         Datatype: URI_Type_Without_Finalize'Class;
                                         Language: RDF.Auxiliary.String_Holders.Holder)
                                         return Term_Type;

   not overriding function From_URI_String (World: Raptor_World_Type_Without_Finalize'Class; URI: URI_String)
                                            return Term_Type;

   not overriding function From_URI (World: Raptor_World_Type_Without_Finalize'Class;
                                     URI: URI_Type_Without_Finalize'Class)
                                     return Term_Type;

   not overriding function From_String (World: Raptor_World_Type_Without_Finalize'Class; Value: String)
                                        return Term_Type;

   subtype URI_Term_Type is Term_Type
     with Dynamic_Predicate => Is_URI(URI_Term_Type);
   subtype Literal_Term_Type is Term_Type
     with Dynamic_Predicate => Is_Literal(Literal_Term_Type);
   subtype Blank_Term_Type is Term_Type
     with Dynamic_Predicate => Is_Blank(Blank_Term_Type);

private

--     type Term_Literal_Value is
--        record
--           str: chars_ptr;
--           Len: unsigned;
--           Datatype: URI_Handle;
--           Language: chars_ptr;
--           Language_Len: unsigned;
--        end record
--       with Convention => C;

--     type Term_Blank_Value is
--        record
--           str: chars_ptr;
--           Len: unsigned;
--        end record
--       with Convention => C;

   type Term_Value (Kind: Term_Kind := Term_Kind'First) is
      record
         case Kind is
            when Unknown => null;
            when URI     => URI: URI_Handle;
            when Literal => Literal: Term_Literal_Value;
            when Blank   => Blank: Term_Blank_Value;
         end case;
      end record
     with Unchecked_Union, Convention => C;

   type Term_Record is
      record
         World: Raptor_World_Handle;
         Usage: Interfaces.C.int; -- intentionally not accessible from our Ada bindings
         Kind: Term_Kind;
         Value: Term_Value;
      end record
     with Convention => C;

end RDF.Raptor.Term;
