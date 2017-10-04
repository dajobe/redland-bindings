with Ada.Iterator_Interfaces;
with RDF.Auxiliary;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Statement; use RDF.Raptor.Statement;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;
with RDF.Rasqal.World; use RDF.Rasqal.World;
with RDF.Rasqal.Literal; use RDF.Rasqal.Literal;
limited with RDF.Rasqal.Query;

package RDF.Rasqal.Query_Results is

   package Query_Results_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Query_Results_Handle is Query_Results_Handled_Record.Access_Type;

   type Query_Results_Type_Without_Finalize is new Query_Results_Handled_Record.Base_Object with null record;

   type Query_Results_Type_Enum is (Results_Bindings,
                                    Results_Boolean,
                                    Results_Graph,
                                    Results_Syntax,
                                    Results_Unknown);

   -- function Add_Row is deliberately not implemented

   not overriding function Finished (Results: Query_Results_Type_Without_Finalize) return Boolean;

   not overriding function Not_Finished (Results: Query_Results_Type_Without_Finalize) return Boolean is
     (not Finished(Results));

   not overriding function Get_Binding_Name (Results: Query_Results_Type_Without_Finalize;
                                             Offset: Natural)
                                             return String;

   not overriding function Get_Binding_Value (Results: Query_Results_Type_Without_Finalize;
                                              Offset: Natural)
                                              return Literal_Type_Without_Finalize;

   not overriding function Get_Binding_Value_By_Name (Results: Query_Results_Type_Without_Finalize;
                                                      Name: String)
                                                      return Literal_Type_Without_Finalize;

   -- rasqal_query_results_get_bindings() deliberately not implemented.
   -- Use iterators instead.

   not overriding function Get_Bindings_Count (Results: Query_Results_Type_Without_Finalize)
                                               return Natural; -- or Positive?

   not overriding function Get_Boolean (Results: Query_Results_Type_Without_Finalize) return Boolean;

   not overriding function Get_Current_Count (Results: Query_Results_Type_Without_Finalize) return Natural;

   not overriding function Get_Query (Results: Query_Results_Type_Without_Finalize)
                                      return RDF.Rasqal.Query.Query_Type_Without_Finalize;

   not overriding function Get_Triple (Results: Query_Results_Type_Without_Finalize)
                                       return Statement_Type_Without_Finalize;

   -- Deliberately not implemented:
   --     function Get_Row_By_Offset (Results: Query_Results_Type_Without_Finalize; Offset: Natural) return XXX;

   not overriding function Get_Type (Results: Query_Results_Type_Without_Finalize)
                                     return Query_Results_Type_Enum;

   not overriding function Is_Bindings (Results: Query_Results_Type_Without_Finalize) return Boolean;
   not overriding function Is_Boolean (Results: Query_Results_Type_Without_Finalize) return Boolean;
   not overriding function Is_Graph (Results: Query_Results_Type_Without_Finalize) return Boolean;
   not overriding function Is_Syntax (Results: Query_Results_Type_Without_Finalize) return Boolean;

   not overriding procedure Next (Results: Query_Results_Type_Without_Finalize);

   not overriding procedure Next_Triple (Results: Query_Results_Type_Without_Finalize);

   not overriding procedure Read (Stream: Base_Stream_Type'Class;
                                  Results: Query_Results_Type_Without_Finalize;
                                  Format_Name: String; -- "" for no format name
                                  Mime_Type: String; -- "" for no MIME type
                                  Format_URI: URI_Type_Without_Finalize'Class;
                                  Base_URI: URI_Type_Without_Finalize'Class);

   not overriding procedure Write (Stream: Base_Stream_Type'Class;
                                   Results: Query_Results_Type_Without_Finalize;
                                   Format_Name: String; -- "" for no format name
                                   Mime_Type: String; -- "" for no MIME type
                                   Format_URI: URI_Type_Without_Finalize'Class;
                                   Base_URI: URI_Type_Without_Finalize'Class);

   not overriding function Type_Label (Kind: Query_Results_Type_Enum) return String;

   not overriding procedure Rewind (Results: in out Query_Results_Type_Without_Finalize);

   type Query_Results_Type is new Query_Results_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: Query_Results_Type; Handle: Query_Results_Handle);

   not overriding function New_Query_Results (World: Rasqal_World_Type_Without_Finalize'Class;
                                              Query: RDF.Rasqal.Query.Query_Type_Without_Finalize;
                                              Kind: Query_Results_Type_Enum)
                                              return Query_Results_Type;

   -- TODO: Not supported as of Rasqal 0.9.32
   --     not overriding function From_String (World: Rasqal_World_Type_Without_Finalize;
   --                                          Kind: Query_Results_Type_Enum;
   --                                          Base_URI: URI_Type_Without_Finalize;
   --                                          Value: String)
   --                                          return Query_Results_Type;

   -- The same cursor is used for bindings iterator and for triples iterator.
   -- However, don't rely on using the same type.
   -- Do not create more than one cursor for the same query results object.
   -- TODO: If in debug mode, enforce this restriction.
   type Cursor is private;

   not overriding function Has_Element (Position: Cursor) return Boolean;

   package Base_Iterators is new Ada.Iterator_Interfaces(Cursor, Has_Element);

   type Bindings_Iterator is new Base_Iterators.Forward_Iterator with private;

   not overriding function Create_Bindings_Iterator (Results: in out Query_Results_Type_Without_Finalize'Class)
                                                     return Bindings_Iterator;

   overriding function First (Object: Bindings_Iterator) return Cursor;

   overriding function Next (Object: Bindings_Iterator; Position: Cursor) return Cursor;

   not overriding function Get_Binding_Value (Position: Cursor;
                                              Offset: Natural)
                                              return Literal_Type_Without_Finalize;

   not overriding function Get_Binding_Value_By_Name (Position: Cursor;
                                                      Name: String)
                                                      return Literal_Type_Without_Finalize;

   type Triples_Iterator is new Base_Iterators.Forward_Iterator with private;

   not overriding function Create_Triples_Iterator (Results: in out Query_Results_Type_Without_Finalize'Class)
                                                    return Bindings_Iterator;

   overriding function First (Object: Triples_Iterator) return Cursor;

   overriding function Next (Object: Triples_Iterator; Position: Cursor) return Cursor;

   not overriding function Get_Triple (Position: Cursor) return Statement_Type_Without_Finalize;

   type Variables_Cursor is private;

   not overriding function Has_Element (Position: Variables_Cursor) return Boolean;

   package Variables_Iterators is new Ada.Iterator_Interfaces(Variables_Cursor, Has_Element);

   -- We can also make backward iterator, but I see no use cases for this
   type Variables_Iterator is new Variables_Iterators.Forward_Iterator with private;

   not overriding function Create_Variables_Iterator (Results: Query_Results_Type_Without_Finalize'Class)
                                                      return Variables_Iterator;

   overriding function First (Object: Variables_Iterator) return Variables_Cursor;

   overriding function Next (Object: Variables_Iterator; Position: Variables_Cursor) return Variables_Cursor;

   not overriding function Get_Name (Position: Variables_Cursor) return String;

   subtype Bindings_Query_Results_Type_Without_Finalize is Query_Results_Type_Without_Finalize
     with Dynamic_Predicate => Is_Bindings(Bindings_Query_Results_Type_Without_Finalize);
   subtype Boolean_Query_Results_Type_Without_Finalize is Query_Results_Type_Without_Finalize
     with Dynamic_Predicate => Is_Boolean(Boolean_Query_Results_Type_Without_Finalize);
   subtype Graph_Query_Results_Type_Without_Finalize is Query_Results_Type_Without_Finalize
     with Dynamic_Predicate => Is_Graph(Graph_Query_Results_Type_Without_Finalize);
   subtype Syntax_Query_Results_Type_Without_Finalize is Query_Results_Type_Without_Finalize
     with Dynamic_Predicate => Is_Syntax(Syntax_Query_Results_Type_Without_Finalize);

   subtype Bindings_Query_Results_Type is Query_Results_Type
     with Dynamic_Predicate => Is_Bindings(Bindings_Query_Results_Type);
   subtype Boolean_Query_Results_Type is Query_Results_Type
     with Dynamic_Predicate => Is_Boolean(Boolean_Query_Results_Type);
   subtype Graph_Query_Results_Type is Query_Results_Type
     with Dynamic_Predicate => Is_Graph(Graph_Query_Results_Type);
   subtype Syntax_Query_Results_Type is Query_Results_Type
     with Dynamic_Predicate => Is_Syntax(Syntax_Query_Results_Type);

private

   type Cursor is access constant Query_Results_Type_Without_Finalize'Class;

   type Bindings_Iterator is new Base_Iterators.Forward_Iterator with
      record
         Ref: Cursor;
      end record;

   type Triples_Iterator is new Base_Iterators.Forward_Iterator with
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

end RDF.Rasqal.Query_Results;
