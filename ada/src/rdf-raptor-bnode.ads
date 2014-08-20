with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Auxilary.C_String_Holders; use RDF.Auxilary.C_String_Holders;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C; use Interfaces.C;
with RDF.Auxilary; use RDF.Auxilary;

package RDF.Raptor.Bnode is

   function Generate_Bnodeid (World: World_Type_Without_Finalize'Class) return String;

   procedure Set_Generate_Bnodeid_Parameters (World: World_Type_Without_Finalize'Class;
                                              Prefix: String_Holders.Holder;
                                              Base: int);

   -- Should we derive it from Limited_Controlled?
   type BNode_ID_Handler is abstract tagged limited private;

   function Do_Handle (Handler: BNode_ID_Handler; User_ID: String) return String is abstract;

   procedure Set_BNode_ID_Handler (World: World_Type_Without_Finalize'Class; Handler: BNode_ID_Handler);

private

   type BNode_ID_Handler is abstract tagged limited null record;

end RDF.Raptor.Bnode;
