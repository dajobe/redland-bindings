with RDF.Redland.Iterator; use RDF.Redland.Iterator;
with RDF.Redland.Node; use RDF.Redland.Node;

package RDF.Redland.Node_Iterator is

   type Node_Iterator_Type_Without_Finalize is new Iterator_Type_Without_Finalize with null record;

   subtype Node_Iterator_Handle is Iterator_Handle;

   not overriding function Get_Node (Iterator: Node_Iterator_Type_Without_Finalize)
                                     return Node_Type_Without_Finalize;

   package Finalizer is new Iterator_Handled_Record.With_Finalization(Node_Iterator_Type_Without_Finalize);

   type Node_Iterator_Type is new Finalizer.Base_With_Finalization with null record;

end RDF.Redland.Node_Iterator;
