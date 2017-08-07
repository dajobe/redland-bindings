with RDF.Auxiliary;
with RDF.Rasqal.World; use RDF.Rasqal.World;

package RDF.Rasqal.Bnode is

   -- TODO: Unfortunate code duplication with RDF.Rasqal.Bnode

   -- Should we derive it from Limited_Controlled?
   type BNode_ID_Handler is abstract tagged limited private;

   function Do_Handle (World: World_Type_Without_Finalize;
                       Handler: BNode_ID_Handler;
                       User_ID: RDF.Auxiliary.String_Holders.Holder)
                       return String is abstract;

   procedure Set_BNode_ID_Handler (World: World_Type_Without_Finalize'Class; Handler: access BNode_ID_Handler'Class);

private

   type BNode_ID_Handler is abstract tagged limited null record;

end RDF.Rasqal.Bnode;
