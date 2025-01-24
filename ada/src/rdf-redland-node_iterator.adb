with Ada.Containers.Vectors;
with RDF.Auxiliary.Convert_Void;
with RDF.Raptor.Term; use RDF.Raptor.Term;

package body RDF.Redland.Node_Iterator is

   package My_Conv is new RDF.Auxiliary.Convert_Void(Term_Record);

   function Get_Node (Iterator: Node_Iterator_Type_Without_Finalize)
                      return Node_Type_Without_Finalize is
      Ptr: constant My_Conv.Object_Pointer := My_Conv.To_Access(Get_Object_Internal(Iterator));
   begin
      return From_Non_Null_Handle(Node_Handle(Ptr));
   end;

   function To_Array (Iterator: in out Node_Iterator_Type_Without_Finalize) return Node_Array is
      package V is new Ada.Containers.Vectors(Ada.Containers.Count_Type, Node_Type);
      Result: V.Vector;
   begin
      while not Is_End(Iterator) loop
         V.Append(Result, Copy(Get_Node(Iterator)));
         Next(Iterator);
      end loop;
      declare
         Result2: Node_Array(1..V.Length(Result));
      begin
         for I in Result2'Range loop
            Result2(I) := Result.Element(I);
         end loop;
         return Result2;
      end;
   end;

end RDF.Redland.Node_Iterator;
