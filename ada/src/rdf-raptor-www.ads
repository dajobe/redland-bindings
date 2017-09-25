with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;

package RDF.Raptor.WWW is

   package WWW_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype WWW_Handle is WWW_Handled_Record.Access_Type;

   -- I deliberately expose that it is an access type (not just a private type),
   -- to simplify libcurl and libxml interaction
   type Connection_Type is new RDF.Auxiliary.Dummy_Record_Access;

   type WWW_Type_Without_Finalize is new WWW_Handled_Record.Base_Object with null record;

   -- You can call this function or initialize only callbacks you need (below).
   not overriding procedure Initialize_All_Callbacks (WWW: WWW_Type_Without_Finalize);

   not overriding procedure Initialize_Write_Bytes_Handler (WWW: WWW_Type_Without_Finalize);
   not overriding procedure Initialize_Content_Type_Handler (WWW: WWW_Type_Without_Finalize);
   not overriding procedure Initialize_URI_Filter (WWW: WWW_Type_Without_Finalize);
   not overriding procedure Initialize_Final_URI_Handler (WWW: WWW_Type_Without_Finalize);

   not overriding procedure Write_Bytes_Handler (WWW: WWW_Type_Without_Finalize; Value: String) is null;
   not overriding procedure Content_Type_Handler (WWW: WWW_Type_Without_Finalize; Content_Type: String) is null;
   not overriding procedure Final_URI_Handler (WWW: WWW_Type_Without_Finalize; URI: URI_Type_Without_Finalize) is null;

   -- Return False to disallow loading an URI
   not overriding function URI_Filter (WWW: WWW_Type_Without_Finalize;
                                       URI: URI_Type_Without_Finalize'Class)
                                       return Boolean is (True);

   -- Empty string means no User-Agent header (I make the behavior the same as --user-agent="" in Wget.
   not overriding procedure Set_User_Agent (WWW: WWW_Type_Without_Finalize; User_Agent: String);

   not overriding procedure Set_Proxy (WWW: WWW_Type_Without_Finalize; Proxy: String);

   -- The same as for User-Agent
   not overriding procedure Set_HTTP_Accept (WWW: WWW_Type_Without_Finalize; Value: String);

   -- Empty Cache_Control is not the same as Unset_Cache_Control
   not overriding procedure Set_Cache_Control (WWW: WWW_Type_Without_Finalize; Cache_Control: String);

   -- Remove Cache-Control: header altogether
   not overriding procedure Unset_Cache_Control (WWW: WWW_Type_Without_Finalize);

   not overriding procedure Set_Connection_Timeout (WWW: WWW_Type_Without_Finalize; Timeout: Natural);

   not overriding function Get_Final_URI (WWW: WWW_Type_Without_Finalize) return URI_Type;

   not overriding procedure Fetch (WWW: WWW_Type_Without_Finalize; URI: URI_Type_Without_Finalize);

   not overriding function Fetch_To_String (WWW: WWW_Type_Without_Finalize; URI: URI_Type_Without_Finalize) return String;

   not overriding function Get_Connection (WWW: WWW_Type_Without_Finalize) return Connection_Type;

   not overriding procedure Set_SSL_Cert_Options (WWW: WWW_Type_Without_Finalize; Cert_Filename, Cert_Type, Cert_Passphrase: String);

   not overriding procedure Set_SSL_Verify_Options (WWW: WWW_Type_Without_Finalize; Verify_Peer, Verify_Host: Boolean);

   not overriding procedure Abort_Operation (WWW: WWW_Type_Without_Finalize; Reason: String);

   type WWW_Type is new WWW_Type_Without_Finalize with null record;

   not overriding function New_WWW (World: Raptor_World_Type'Class) return WWW_Type;

   not overriding function New_WWW (World: Raptor_World_Type'Class; Connection: Connection_Type) return WWW_Type;

   overriding procedure Finalize_Handle (Object: WWW_Type; Handle: WWW_Handle);

end RDF.Raptor.WWW;
