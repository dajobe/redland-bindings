with Ada.Unchecked_Conversion;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.C_String_Holders; use RDF.Auxiliary.C_String_Holders;
with RDF.Raptor.Memory;

package body RDF.Raptor.Bnode is

   function raptor_world_generate_bnodeid (World: Raptor_World_Handle) return Chars_Ptr
     with Import, Convention=>C;

   function Generate_Bnodeid (World: Raptor_World_Type_Without_Finalize'Class) return String is
      C_Str: constant chars_ptr := raptor_world_generate_bnodeid(Get_Handle(World));
      Result: constant String := Value(C_Str);
   begin
      RDF.Raptor.Memory.raptor_free_memory(C_Str);
      return Result;
   end;

   procedure raptor_world_set_generate_bnodeid_parameters (World: Raptor_World_Handle; Prefix: chars_ptr; Base: int)
     with Import, Convention=>C;

   procedure Set_Generate_Bnodeid_Parameters (World: Raptor_World_Type_Without_Finalize'Class;
                                              Prefix: String_Holders.Holder;
                                              Base: int) is
      Prefix_N: constant C_String_Holder := To_C_String_Holder(Prefix);
   begin
      raptor_world_set_generate_bnodeid_parameters(Get_Handle(World), C_String(Prefix_N), Base);
   end;

   type C_BNode_ID_Handler is access function (Data: chars_ptr; User_ID: chars_ptr) return Chars_Ptr
     with Convention=>C;

   type User_Defined_Access is access constant BNode_ID_Handler'Class;
   function Ptr_To_Obj is new Ada.Unchecked_Conversion(chars_ptr, User_Defined_Access);
   function Obj_To_Ptr is new Ada.Unchecked_Conversion(User_Defined_Access, chars_ptr);

   function C_BNode_ID_Handle_Impl(Data: chars_ptr; User_ID: chars_ptr) return Chars_Ptr
      with Convention=>C;

   function C_BNode_ID_Handle_Impl(Data: chars_ptr; User_ID: chars_ptr) return Chars_Ptr is
      use RDF.Auxiliary.String_Holders;
      User_ID2: RDF.Auxiliary.String_Holders.Holder;
   begin
      if User_ID /= Null_Ptr then
         Replace_Element(User_ID2, Value(User_ID));
      end if;
      declare
         Result: constant Chars_Ptr := New_String(Do_Handle(Ptr_To_Obj(Data).all, User_ID2));
      begin
         RDF.Raptor.Memory.raptor_free_memory(User_ID);
         return Result;
      end;
   end;

   procedure raptor_world_set_generate_bnodeid_handler (World: Raptor_World_Handle; Data: chars_ptr; Handler: C_BNode_ID_Handler)
     with Import, Convention=>C;

   procedure Set_BNode_ID_Handler (World: Raptor_World_Type_Without_Finalize'Class; Handler: access BNode_ID_Handler'Class) is
   begin
      raptor_world_set_generate_bnodeid_handler(Get_Handle(World), Obj_To_Ptr(Handler), C_BNode_ID_Handle_Impl'Access);
   end;

end RDF.Raptor.Bnode;
