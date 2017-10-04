with RDF.Raptor.World; use RDF.Raptor.World;
with Interfaces.C; use Interfaces.C;
with RDF.Auxiliary; use RDF.Auxiliary;

package RDF.Raptor.Bnode is

   function Generate_Bnodeid (World: Raptor_World_Type_Without_Finalize'Class) return String;

   procedure Set_Generate_Bnodeid_Parameters (World: Raptor_World_Type_Without_Finalize'Class;
                                              Prefix: String_Holders.Holder;
                                              Base: int);

   -- Should we derive it from Limited_Controlled?
   type BNode_ID_Handler is abstract tagged limited private;

   function Do_Handle (Handler: BNode_ID_Handler; User_ID: RDF.Auxiliary.String_Holders.Holder)
                       return String is abstract;

   procedure Set_BNode_ID_Handler (World: in out Raptor_World_Type_Without_Finalize'Class;
                                   Handler: access BNode_ID_Handler'Class);

private

   type BNode_ID_Handler is abstract tagged limited null record;

end RDF.Raptor.Bnode;
