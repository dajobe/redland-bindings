with RDF.Auxilary.Handled_Record;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxilary;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;

package RDF.Raptor.Statement is

   type Term_Record is private;

   package Term_Handled_Record is new RDF.Auxilary.Handled_Record(Term_Record);

   type Term_Type_Without_Finalize is new Term_Handled_Record.Base_Object with null record;

   subtype Term_Handle is Term_Handled_Record.Access_Type;

   type Term_Literal_Value is private;

   type Term_Blank_Value is private;

   type Term_Kind is (Unknown,
                      URI,
                      Literal,
                      Blank);
   for Term_Kind'Size use Interfaces.C.int'Size; -- hack
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
   not overriding function Datatype (Literal: Term_Literal_Value) return RDF.Raptor.URI.URI_Type;

   -- TODO: Not sure that it does the right thing with empty language string.
   --
   -- Return the language tag or empty string if there are none
   not overriding function Language (Literal: Term_Literal_Value) return String;

   not overriding function Value (Blank: Term_Blank_Value) return String;

   type Term_Type is new Term_Type_Without_Finalize with null record;

   not overriding function From_Blank (World: World_Type_Without_Finalize'Class) return Term_Type;
   not overriding function From_Blank (World: World_Type_Without_Finalize'Class; ID: String) return Term_Type;
--     not overriding function From_Blank (World: World_Type_Without_Finalize'Class; ID: RDF.Auxilary.String_Or_Null) return Term_Type; -- TODO: uncomment

   not overriding function From_Literal (World   : World_Type_Without_Finalize'Class;
                                         Literal : RDF.Auxilary.String_Holders.Holder;
                                         Datatype: RDF.Raptor.URI.URI_Type_Without_Finalize'Class;
                                         Language: RDF.Auxilary.String_Holders.Holder)
                                         return Term_Type;

   -- TODO: Stopped at raptor_new_term_from_counted_literal() (with an error)

--     type Raptor_Statement_Record is private;

--     package My_Limited_Handled_Record is
--       new RDF.Auxilary.Limited_Handled_Record(Raptor_Statement_Record, Raptor_Statement_Record_Access);
--
--     subtype Raptor_Statement is My_Limited_Handled_Record.Base_Object;
--
--     type Raptor_Statement_Handler is abstract tagged null record;
--
--     procedure Handle (Handler: Raptor_Statement_Handler) is abstract;
--
--     procedure C_Handler (User_Data: RDF.Auxilary.Dummy_Record_Access; Statement: Raptor_Statement_Record_Access);

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

--     type Raptor_Statement_Record is
--        record
--           world: access RDF.Raptor.World.World_Type;
--           usage: Interfaces.C.int;
--           Subject, Predicate, Object, Graph: access Raptor_Term;
--        end record
--           with Convention=>C;

end RDF.Raptor.Statement;
