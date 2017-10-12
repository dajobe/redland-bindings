with RDF.Redland.Iterator; use RDF.Redland.Iterator;
with RDF.Redland.Node; use RDF.Redland.Node;

package RDF.Redland.Node_Iterator is

   -- I do not defined Node_Iterator_Without_Finalize,
   -- as we do not really need it

   type Node_Iterator_Type is new Iterator_Type with null record;

   not overriding function Get_Node (Iterator: Node_Iterator_Type) return Node_Type_Without_Finalize;

end RDF.Redland.Node_Iterator;
