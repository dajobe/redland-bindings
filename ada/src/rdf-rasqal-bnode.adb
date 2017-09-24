with Ada.Unchecked_Conversion;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.C_String_Holders;
with RDF.Rasqal.Memory;

package body RDF.Rasqal.Bnode is

   type C_BNode_ID_Handler is access function (World: RDF.Rasqal.World.Handle_Type;
                                               Data: chars_ptr;
                                               User_ID: chars_ptr)
                                               return Chars_Ptr
     with Convention=>C;

   type User_Defined_Access is access constant BNode_ID_Handler'Class;
   function Ptr_To_Obj is new Ada.Unchecked_Conversion(chars_ptr, User_Defined_Access);
   function Obj_To_Ptr is new Ada.Unchecked_Conversion(User_Defined_Access, chars_ptr);

   function C_BNode_ID_Handle_Impl(World: RDF.Rasqal.World.Handle_Type;
                                   Data: chars_ptr;
                                   User_ID: chars_ptr)
                                   return Chars_Ptr
      with Convention=>C;

   function C_BNode_ID_Handle_Impl(World: RDF.Rasqal.World.Handle_Type;
                                   Data: chars_ptr;
                                   User_ID: chars_ptr) return Chars_Ptr is
      use RDF.Auxiliary.String_Holders;
      User_ID2: RDF.Auxiliary.String_Holders.Holder;
   begin
      if User_ID /= Null_Ptr then
         Replace_Element(User_ID2, Value(User_ID));
      end if;
      declare
         use RDF.Rasqal.World;
         Result: constant Chars_Ptr := New_String(Do_Handle(Rasqal_World_Type_Without_Finalize'(From_Non_Null_Handle(World)),
                                                            Ptr_To_Obj(Data).all,
                                                            User_ID2));
      begin
         RDF.Rasqal.Memory.rasqal_free_memory(User_ID);
         return Result;
      end;
   end;

   procedure rasqal_world_set_generate_bnodeid_handler (World: RDF.Rasqal.World.Handle_Type; Data: chars_ptr; Handler: C_BNode_ID_Handler)
     with Import, Convention=>C;

   procedure Set_BNode_ID_Handler (World: Rasqal_World_Type_Without_Finalize'Class; Handler: access BNode_ID_Handler'Class) is
   begin
      rasqal_world_set_generate_bnodeid_handler(Get_Handle(World), Obj_To_Ptr(Handler), C_BNode_ID_Handle_Impl'Access);
   end;

   function rasqal_world_set_default_generate_bnodeid_parameters (World: RDF.Rasqal.World.Handle_Type;
                                                                  Prefix: Chars_Ptr;
                                                                  Base: int)
                                                                  return int
     with Import, Convention=>C;

   procedure Set_Default_Generate_Bnodeid_Parameters (World: Rasqal_World_Type_Without_Finalize'Class;
                                                      Prefix: String_Holders.Holder;
                                                      Base: int) is
      use RDF.Auxiliary.C_String_Holders;
      C_Prefix: constant chars_ptr := New_String(Prefix);
      Result: constant int := rasqal_world_set_default_generate_bnodeid_parameters(Get_Handle(World), C_Prefix, Base);
   begin
      RDF.Rasqal.Memory.Rasqal_Free_Memory(C_Prefix);
      if Result /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

end RDF.Rasqal.Bnode;
