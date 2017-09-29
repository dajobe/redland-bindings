with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.Log; use RDF.Raptor.Log;
with RDF.Raptor.URI; use RDF.Raptor.URI;

package RDF.Rasqal.World is

   package Rasqal_World_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Rasqal_World_Type_Without_Finalize is new Rasqal_World_Handled_Record.Base_Object with null record;

   subtype Rasqal_World_Handle is Rasqal_World_Handled_Record.Access_Type;

   overriding function Default_Handle(Object: Rasqal_World_Type_Without_Finalize) return Rasqal_World_Handle;

   not overriding procedure Open(Object: Rasqal_World_Type_Without_Finalize);

   not overriding procedure Set_Log_Handler(World: Rasqal_World_Type_Without_Finalize; Handler: access Log_Handler'Class);

   type Warning_Level is range 0 .. 100;

   not overriding procedure Set_Warning_Level (World: Rasqal_World_Type_Without_Finalize; Level: Warning_Level);

   not overriding function Get_Raptor (World: Rasqal_World_Type_Without_Finalize) return Raptor_World_Type_Without_Finalize;

   not overriding procedure Set_Raptor (World: Rasqal_World_Type_Without_Finalize; Raptor_World: Raptor_World_Type_Without_Finalize'Class);

   not overriding function Guess_Query_Results_Format_Name (World: Rasqal_World_Type_Without_Finalize;
                                                            URI: URI_Type_Without_Finalize'Class;
                                                            Mime_Type: String_Holders.Holder;
                                                            Buffer: String_Holders.Holder;
                                                            Identifier: String_Holders.Holder)
                                                            return String_Holders.Holder;

   type Rasqal_World_Type is new Rasqal_World_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: Rasqal_World_Type; Handle: Rasqal_World_Handle);

   not overriding function Open return Rasqal_World_Type;

end RDF.Rasqal.World;
