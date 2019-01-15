with Ada.Containers.Indefinite_Holders;
with Ada.Iterator_Interfaces;
with Interfaces.C; use Interfaces.C;
with RDF.Auxiliary;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.URI; use RDF.Redland.URI;
with RDF.Redland.Node; use RDF.Redland.Node;
with RDF.Redland.Statement; use RDF.Redland.Statement;
with RDF.Redland.Stream; use RDF.Redland.Stream;
with RDF.Redland.Storage; use RDF.Redland.Storage;
with RDF.Redland.Node_Iterator; use RDF.Redland.Node_Iterator;
with RDF.Redland.Query; use RDF.Redland.Query;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;
with RDF.Redland.Query_Results; use RDF.Redland.Query_Results;

package RDF.Redland.Model is

   -- Use Limited_Handled_Record because despite a model can be copied,
   -- copying may be a very expensive operation. Use Copy function instead.
   -- See also https://gcc.gnu.org/bugzilla/show_bug.cgi?id=62042
   package Model_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Model_Type_Without_Finalize is new Model_Handled_Record.Base_Object with null record;

   subtype Model_Handle is Model_Handled_Record.Access_Type;

   overriding procedure Finalize_Handle (Object: Model_Type_Without_Finalize; Handle: Model_Handle);

   type Model_Info (Name_Length, Label_Length: Natural) is
      record
         Name : String(1..Name_Length );
         Label: String(1..Label_Length);
      end record;

   package Model_Info_Holders is new Ada.Containers.Indefinite_Holders(Model_Info);

   function Enumerate_Models (World: Redland_World_Type_Without_Finalize'Class;
                              Counter: unsigned)
                              return Model_Info_Holders.Holder;

   type Enumerate_Models_Cursor is private;

   function Has_Element (Position: Enumerate_Models_Cursor) return Boolean;

   package Enumerate_Models_Interfaces is
     new Ada.Iterator_Interfaces(Enumerate_Models_Cursor, Has_Element);

   type Enumerate_Models_Iterator is new Enumerate_Models_Interfaces.Forward_Iterator with private;

   overriding function First (Object: Enumerate_Models_Iterator) return Enumerate_Models_Cursor;

   overriding function Next (Object: Enumerate_Models_Iterator; Position: Enumerate_Models_Cursor)
                             return Enumerate_Models_Cursor;

   -- FIXME: Forgotten to retrieve data from Enumerate_Models_Cursor (now this works only in D)

   not overriding function Size_Without_Exception (Model: Model_Type_Without_Finalize)
                                                   return Integer;

   not overriding function Size (Model: Model_Type_Without_Finalize) return Natural;

   not overriding procedure Add (Model: in out Model_Type_Without_Finalize;
                                 Subject, Predicate, Object: Node_Type_Without_Finalize'Class);

   not overriding procedure Add (Model: in out Model_Type_Without_Finalize;
                                 Statement: Statement_Type_Without_Finalize'Class;
                                 Context: Node_Type_Without_Finalize'Class := Node_Type_Without_Finalize'(From_Handle(null)))
     with Pre => Is_Complete(Statement);

   not overriding procedure Add (Model: in out Model_Type_Without_Finalize;
                                 Statements: Stream_Type_Without_Finalize'Class;
                                 Context: Node_Type_Without_Finalize'Class := Node_Type_Without_Finalize'(From_Handle(null)));

   not overriding procedure Remove (Model: Model_Type_Without_Finalize;
                                    Statement: Statement_Type_Without_Finalize'Class);

   not overriding procedure Remove (Model: in out Model_Type_Without_Finalize;
                                    Statement: Statement_Type_Without_Finalize'Class;
                                    Context: Node_Type_Without_Finalize'Class := Node_Type_Without_Finalize'(From_Handle(null)))
   with Pre => Is_Complete(Statement);

   not overriding procedure Remove (Model: in out Model_Type_Without_Finalize;
                                    Statements: Stream_Type_Without_Finalize'Class;
                                    Context: Node_Type_Without_Finalize'Class := Node_Type_Without_Finalize'(From_Handle(null)));

   not overriding function Contains (Model: Model_Type_Without_Finalize;
                                     Statement: Statement_Type_Without_Finalize'Class)
                                     return Boolean;

   not overriding function Has_Arc_In (Model: Model_Type_Without_Finalize;
                                       Node, Property: Node_Type_Without_Finalize'Class)
                                       return Boolean;

   not overriding function Has_Arc_Out (Model: Model_Type_Without_Finalize;
                                        Node, Property: Node_Type_Without_Finalize'Class)
                                        return Boolean;

   not overriding function As_Stream (Model: Model_Type_Without_Finalize) return Stream_Type;

   not overriding function Find (Model: Model_Type_Without_Finalize;
                                 Statement: Statement_Type_Without_Finalize'Class)
                                 return Stream_Type;

   -- LIBRDF_MODEL_FIND_OPTION_MATCH_SUBSTRING_LITERAL not implemented.

   -- librdf_model_find_statements_with_options() not implemented as requires rdf_hash.

   not overriding function Get_Sources (Model: Model_Type_Without_Finalize;
                                        Arc, Target: Node_Type_Without_Finalize'Class)
                                        return Node_Iterator_Type;

   not overriding function Get_Arcs (Model: Model_Type_Without_Finalize;
                                     Source, Target: Node_Type_Without_Finalize'Class)
                                     return Node_Iterator_Type;

   not overriding function Get_Targets (Model: Model_Type_Without_Finalize;
                                        Source, Arc: Node_Type_Without_Finalize'Class)
                                        return Node_Iterator_Type;

   not overriding function Get_Source (Model: Model_Type_Without_Finalize;
                                       Arc, Target: Node_Type_Without_Finalize'Class)
                                       return Node_Type;

   not overriding function Get_Arc (Model: Model_Type_Without_Finalize;
                                    Source, Target: Node_Type_Without_Finalize'Class)
                                    return Node_Type;

   not overriding function Get_Target (Model: Model_Type_Without_Finalize;
                                       Source, Arc: Node_Type_Without_Finalize'Class)
                                       return Node_Type;

   not overriding procedure Add_Submodel (Model: Model_Type_Without_Finalize;
                                          Submodel: Model_Type_Without_Finalize'Class);

   not overriding procedure Remove_Submodel (Model: Model_Type_Without_Finalize;
                                             Submodel: Model_Type_Without_Finalize'Class);

   not overriding function Context_As_Stream (Model: Model_Type_Without_Finalize;
                                              Context: Node_Type_Without_Finalize'Class)
                                              return Stream_Type;

   not overriding function Contains_Context (Model: Model_Type_Without_Finalize;
                                             Context: Node_Type_Without_Finalize'Class)
                                             return Boolean;

   not overriding function Supports_Context (Model: Model_Type_Without_Finalize) return Boolean;

   not overriding function Query_Execute (Model: Model_Type_Without_Finalize;
                                          Query: Query_Type_Without_Finalize'Class)
                                          return Query_Results_Type_Without_Finalize;

   not overriding procedure Sync (Model: Model_Type_Without_Finalize);

   not overriding function Get_Storage (Model: Model_Type_Without_Finalize)
                                        return Storage_Type_Without_Finalize;

   not overriding procedure Load (Model: Model_Type_Without_Finalize;
                                  URI: URI_Type_Without_Finalize'Class;
                                  Name, Mime_Type: String := "";
                                  Type_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)));

   not overriding function To_String (Model: Model_Type_Without_Finalize;
                                      URI: URI_Type_Without_Finalize'Class;
                                      Name, Mime_Type: String := "";
                                      Type_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                                      return String;

   not overriding function Find_In_Context (Model: Model_Type_Without_Finalize;
                                            Statement: Statement_Type_Without_Finalize'Class;
                                            Context: Node_Type_Without_Finalize'Class)
                                            return Stream_Type;

   not overriding function Get_Contexts (Model: Model_Type_Without_Finalize) return Node_Iterator_Type;

   Feature_Contexts: constant URI_String := "http://feature.librdf.org/model-contexts";

   not overriding function Get_Feature (Model: Model_Type_Without_Finalize;
                                        Feature: URI_Type_Without_Finalize'Class)
                                        return Node_Type;

   not overriding procedure Set_Feature (Model: in out Model_Type_Without_Finalize;
                                         Feature: URI_Type_Without_Finalize'Class;
                                         Value: Node_Type);

   not overriding procedure Add_String_Literal_Statement (Model: Model_Type_Without_Finalize;
                                                          Subject, Predicate: Node_Type_Without_Finalize'Class;
                                                          Literal: String;
                                                          Language: String;
                                                          Is_XML: Boolean := False);

   not overriding procedure Add_Typed_Literal_Statement (Model: Model_Type_Without_Finalize;
                                                         Subject, Predicate: Node_Type_Without_Finalize'Class;
                                                         Literal: String;
                                                         Language: String;
                                                         Datatype: URI_Type_Without_Finalize'Class);

   not overriding procedure Write (Model: Model_Type_Without_Finalize; Stream: IOStream_Type_Without_Finalize'Class);

   package Handlers is new Model_Handled_Record.Common_Handlers(Model_Type_Without_Finalize);

   type Model_Type is new Handlers.Base_With_Finalization with null record;

   type Model_Type_User is new Handlers.User_Type with null record;

   not overriding function Create (World: Redland_World_Type_Without_Finalize'Class;
                                   Storage: Storage_Type_Without_Finalize'Class;
                                   Options: String := "")
                                   return Model_Type;

   -- librdf_new_model_with_options() not implemented, because librdf_hash is not implemented

   not overriding function Copy (Model: Model_Type_Without_Finalize'Class) return Model_Type;

private

   type Enumerate_Models_Cursor is
      record
         World: Redland_World_Handle;
         Counter: unsigned;
      end record;

   type Enumerate_Models_Iterator is new Enumerate_Models_Interfaces.Forward_Iterator with
      record
         World: Redland_World_Handle;
      end record;

end RDF.Redland.Model;
