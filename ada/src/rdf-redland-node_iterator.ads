with Ada.Containers;
with RDF.Redland.Iterator; use RDF.Redland.Iterator;
with RDF.Redland.Node; use RDF.Redland.Node;

package RDF.Redland.Node_Iterator is

   type Node_Iterator_Type_Without_Finalize is new Iterator_Type_Without_Finalize with null record;

   subtype Node_Iterator_Handle is Iterator_Handle;

   not overriding function Get_Node (Iterator: Node_Iterator_Type_Without_Finalize)
                                     return Node_Type_Without_Finalize;

   type Node_Array is array (Ada.Containers.Count_Type range <>) of Node_Type;

   -- Note that after this operation Iterator is not usable
   not overriding function To_Array (Iterator: in out Node_Iterator_Type_Without_Finalize)
                                     return Node_Array;

   package Handlers is new Iterator_Handled_Record.Common_Handlers(Node_Iterator_Type_Without_Finalize);

   type Node_Iterator_Type is new Handlers.Base_With_Finalization with null record;

   type Node_Iterator_Type_User is new Handlers.User_Type with null record;

end RDF.Redland.Node_Iterator;
