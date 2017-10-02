with Interfaces.C; use Interfaces.C;
with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Rasqal.World; use RDF.Rasqal.World;

package RDF.Rasqal.Bnode is

   -- Unfortunate code duplication with RDF.Rasqal.Bnode

   -- Should we derive it from Limited_Controlled?
   type BNode_ID_Handler is abstract tagged limited private;

   -- We pass to the underlying C library only a handle of a world,
   -- not the entire object. So it does not behave well in Ada types.
   -- So I recommend not to use World argument at all.
   -- TODO: Remove World argument and make the API the same as in
   -- RDF.Raptor.Bnode, possibly merging both tagged types.
   function Do_Handle (World: Rasqal_World_Type_Without_Finalize;
                       Handler: BNode_ID_Handler;
                       User_ID: RDF.Auxiliary.String_Holders.Holder)
                       return String is abstract;

   procedure Set_BNode_ID_Handler (World: in out Rasqal_World_Type_Without_Finalize'Class; Handler: access BNode_ID_Handler'Class);

   procedure Set_Default_Generate_Bnodeid_Parameters (World: in out Rasqal_World_Type_Without_Finalize'Class;
                                                      Prefix: String_Holders.Holder;
                                                      Base: int);

private

   type BNode_ID_Handler is abstract tagged limited null record;

end RDF.Rasqal.Bnode;
