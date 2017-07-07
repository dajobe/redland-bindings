with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.World;

package RDF.Raptor.WWW is

   package WWW_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype WWW_Handle_Type is WWW_Handled_Record.Access_Type;

   -- I deliberately expose that it is an access type (not just a private type),
   -- to simplify libcurl and libxml interaction
   type Connection_Type is new RDF.Auxiliary.Dummy_Record_Access;

   type WWW_Type_Without_Finalize is new WWW_Handled_Record.Base_Object with null record;

   -- Empty string means no User-Agent header (I make the behavior the same as --user-agent="" in Wget.
   not overriding procedure Set_User_Agent (WWW: WWW_Type_Without_Finalize; User_Agent: String);

   not overriding procedure Set_Proxy (WWW: WWW_Type_Without_Finalize; Proxy: String);

   -- The same as for User-Agent
   not overriding procedure Set_HTTP_Accept (WWW: WWW_Type_Without_Finalize; Value: String);

   -- TODO: Stopped at raptor_www_set_http_cache_control()

   type WWW_Type is new WWW_Type_Without_Finalize with null record;

   not overriding function New_WWW (World: RDF.Raptor.World.World_Type'Class) return WWW_Type;

   not overriding function New_WWW (World: RDF.Raptor.World.World_Type'Class; Connection: Connection_Type) return WWW_Type;

   overriding procedure Finalize_Handle(Object: WWW_Type; Handle: WWW_Handle_Type);

end RDF.Raptor.WWW;
