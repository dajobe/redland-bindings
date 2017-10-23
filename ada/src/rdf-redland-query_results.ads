with Ada.Iterator_Interfaces;
with Ada.Containers.Indefinite_Vectors;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Redland.URI; use RDF.Redland.URI;
with RDF.Redland.Node; use RDF.Redland.Node;
with RDF.Redland.Stream; use RDF.Redland.Stream;

package RDF.Redland.Query_Results is

   package Query_Results_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Query_Results_Type_Without_Finalize is new RDF.Redland.Query_Results.Query_Results_Handled_Record.Base_Object with null record;

   subtype Query_Results_Handle is Query_Results_Handled_Record.Access_Type;

   subtype Bindings_Query_Results_Type_Without_Finalize is Query_Results_Type_Without_Finalize
     with Dynamic_Predicate => Is_Bindings(Bindings_Query_Results_Type_Without_Finalize);
   subtype Boolean_Query_Results_Type_Without_Finalize is Query_Results_Type_Without_Finalize
     with Dynamic_Predicate => Is_Boolean(Boolean_Query_Results_Type_Without_Finalize);
   subtype Graph_Query_Results_Type_Without_Finalize is Query_Results_Type_Without_Finalize
     with Dynamic_Predicate => Is_Graph(Graph_Query_Results_Type_Without_Finalize);
   subtype Syntax_Query_Results_Type_Without_Finalize is Query_Results_Type_Without_Finalize
     with Dynamic_Predicate => Is_Syntax(Syntax_Query_Results_Type_Without_Finalize);

   overriding procedure Finalize_Handle (Object: Query_Results_Type_Without_Finalize;
                                         Handle: Query_Results_Handle);

   not overriding function As_Stream (Results: Query_Results_Type_Without_Finalize) return Stream_Type;

   not overriding function Get_Current_Count (Results: Query_Results_Type_Without_Finalize) return Natural;

   procedure Next (Results: Query_Results_Type_Without_Finalize'Class)
     with Pre => Is_Bindings(Results) or Is_Graph(Results);

   not overriding function Finished (Results: Query_Results_Type_Without_Finalize) return Boolean
     with Pre'Class => Is_Bindings(Results) or Is_Graph(Results);

   not overriding function Not_Finished (Results: Query_Results_Type_Without_Finalize) return Boolean is
     (not Finished(Results));

   function Get_Bindings_Count (Results: Bindings_Query_Results_Type_Without_Finalize'Class)
                                return Natural; -- or shall we use Positive?

   package String_Lists is new Ada.Containers.Indefinite_Vectors(Natural, String);
   type String_List_Type is new String_Lists.Vector with null record;

   function Get_Binding_Names (Results: Bindings_Query_Results_Type_Without_Finalize'Class)
                               return String_List_Type;

   package Node_Lists is new Ada.Containers.Indefinite_Vectors(Natural, Node_Type);
   type Node_List_Type is new Node_Lists.Vector with null record;

   function Get_Binding_Values (Results: Bindings_Query_Results_Type_Without_Finalize'Class)
                                return Node_List_Type;

   function Get_Binding_Value (Results: Bindings_Query_Results_Type_Without_Finalize'Class; Index: Natural)
                               return Node_Type;

   function Get_Binding_Name (Results: Bindings_Query_Results_Type_Without_Finalize'Class; Index: Natural)
                              return String;

   function Get_Binding_Value_By_Name (Results: Bindings_Query_Results_Type_Without_Finalize'Class; Name: String)
                                       return Node_Type;

   not overriding
   function To_String (Results: Query_Results_Type_Without_Finalize;
                       Name: String := "";
                       Mime_Type: String := "";
                       Format_URI, Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                       return String;

   not overriding
   procedure To_File_Handle (Results: Query_Results_Type_Without_Finalize;
                             File: RDF.Auxiliary.C_File_Access;
                             Name: String := "";
                             Mime_Type: String := "";
                             Format_URI, Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)));

   -- librdf_query_results_to_file2() is wrong: http://bugs.librdf.org/mantis/view.php?id=639

   not overriding function Is_Bindings (Results: Query_Results_Type_Without_Finalize) return Boolean;
   not overriding function Is_Boolean  (Results: Query_Results_Type_Without_Finalize) return Boolean;
   not overriding function Is_Graph    (Results: Query_Results_Type_Without_Finalize) return Boolean;
   not overriding function Is_Syntax   (Results: Query_Results_Type_Without_Finalize) return Boolean;

   function Get_Boolean (Results: Boolean_Query_Results_Type_Without_Finalize'Class) return Boolean;

   -- I was lazy to implement query_results_formatter and related functions

   package Finalizer is new Query_Results_Handled_Record.With_Finalization(Query_Results_Type_Without_Finalize);

   -- The same cursor is used for bindings iterator and for Graph iterator.
   -- However, don't rely on using the same type.
   -- Do not create more than one cursor for the same query results object.
   type Cursor is private;

   not overriding function Has_Element (Position: Cursor) return Boolean;

   package Base_Iterators is new Ada.Iterator_Interfaces(Cursor, Has_Element);

   type Bindings_Iterator is new Base_Iterators.Forward_Iterator with private;

   not overriding function Create_Bindings_Iterator (Results: in out Bindings_Query_Results_Type_Without_Finalize'Class)
                                                     return Bindings_Iterator;

   overriding function First (Object: Bindings_Iterator) return Cursor;

   overriding function Next (Object: Bindings_Iterator; Position: Cursor) return Cursor;

   not overriding function Get_Binding_Value (Position: Cursor;
                                              Offset: Natural)
                                              return Node_Type;

   not overriding function Get_Binding_Value_By_Name (Position: Cursor;
                                                      Name: String)
                                                      return Node_Type;

   type Query_Results_Type is new Finalizer.Derived with null record;

   subtype Bindings_Query_Results_Type is Query_Results_Type
     with Dynamic_Predicate => Is_Bindings(Bindings_Query_Results_Type);
   subtype Boolean_Query_Results_Type is Query_Results_Type
     with Dynamic_Predicate => Is_Boolean(Boolean_Query_Results_Type);
   subtype Graph_Query_Results_Type is Query_Results_Type
     with Dynamic_Predicate => Is_Graph(Graph_Query_Results_Type);
   subtype Syntax_Query_Results_Type is Query_Results_Type
     with Dynamic_Predicate => Is_Syntax(Syntax_Query_Results_Type);

   type Variables_Cursor is private;

   not overriding function Has_Element (Position: Variables_Cursor) return Boolean;

   package Variables_Iterators is new Ada.Iterator_Interfaces(Variables_Cursor, Has_Element);

   -- We can also make backward iterator, but I see no use cases for this
   type Variables_Iterator is new Variables_Iterators.Forward_Iterator with private;

   not overriding function Create_Variables_Iterator (Results: Bindings_Query_Results_Type_Without_Finalize'Class)
                                                      return Variables_Iterator;

   overriding function First (Object: Variables_Iterator) return Variables_Cursor;

   overriding function Next (Object: Variables_Iterator; Position: Variables_Cursor) return Variables_Cursor;

   not overriding function Get_Name (Position: Variables_Cursor) return String;

private

   type Cursor is access constant Query_Results_Type_Without_Finalize'Class;

   type Bindings_Iterator is new Base_Iterators.Forward_Iterator with
      record
         Ref: Cursor;
      end record;

   type Variables_Cursor is
      record
         Ref: Cursor; -- hack
         Count: Natural;
      end record;

   type Variables_Iterator is new Variables_Iterators.Forward_Iterator with
      record
         Ref: Cursor;
      end record;

end RDF.Redland.Query_Results;
