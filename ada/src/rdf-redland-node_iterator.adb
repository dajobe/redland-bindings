with RDF.Auxiliary.Convert_Void;
with RDF.Raptor.Term; use RDF.Raptor.Term;

package body RDF.Redland.Node_Iterator is

   package My_Conv is new RDF.Auxiliary.Convert_Void(Term_Record);

   function Get_Node (Iterator: Node_Iterator_Type) return Node_Type_Without_Finalize is
      Ptr: constant My_Conv.Object_Pointer := My_Conv.To_Access(Get_Object_Internal(Iterator));
   begin
      return From_Non_Null_Handle(Node_Handle(Ptr));
   end;

end RDF.Redland.Node_Iterator;
