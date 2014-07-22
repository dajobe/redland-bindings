with Interfaces.C;
with RDF.Base;

package RDF.Raptor is

   type World is new RDF.Base.Base_Object with null record;

   type Flag_Type is (Libxml_Error_Save,
                      Libxml_Structured_Error_Save,
		      URI_Interning,
                      WWW_Skip_Init_Finish);
   for Flag_Type'Size use Interfaces.C.int'Size; -- hack
   for Flag_Type use (Libxml_Error_Save => 1,
                      Libxml_Structured_Error_Save => 2,
		      URI_Interning => 3,
                      WWW_Skip_Init_Finish => 4);

   type Flag_And_Value is
      record
         Flag : Flag_Type;
         Value: Boolean;
      end record;

   type Flags_Array is array(Integer range <>) of Flag_And_Value;

   overriding function Default_Handle(Object: World) return RDF.Base.Dummy_Record_Access;

   --overriding function From_Handle(Handle: RDF.Base.Dummy_Record_Access) return World with Inline;

   not overriding procedure Open(Object: World);

   not overriding procedure Open(Object: World; Flags: Flags_Array);

   not overriding function Open return World;

   not overriding function Open(Flags: Flags_Array) return World;

   overriding procedure Finalize_Handle(Object: World; Handle: RDF.Base.Dummy_Record_Access);

   not overriding procedure Set_Flag(Object: World; Flag: Flag_Type; Value: Boolean);

   not overriding procedure Set_Flags(Object: World; Flags: Flags_Array);

   -- Not implemented
   -- procedure Set_Libxslt_Security_Preferences

   -- FIXME: The below is not thoroughly cheched, maybe it should move to private

   type Domain_Type is (None,
                        Iostream,
                        Namespace,
                        Parser,
                        Qname,
                        Sax2,
                        Serializer,
                        Term,
                        Turtle_Writer,
                        URI,
                        World_Domain,
                        WWW,
                        XML_Writer);
   for Domain_Type'Size use Interfaces.C.int'Size; -- hack
   function Last return Domain_Type is (XML_Writer);

   type Log_Level_Type is (None, Trace, Debug, Info, Warn, Error, Fatal);
   for Log_Level_Type'Size use Interfaces.C.int'Size; -- hack
   function Last return Log_Level_Type is (Fatal);

   type Log_Handler is abstract tagged null record;

   type Log_Message_Type is
      record
         Code: Interfaces.C.int;
         -- TODO
      end record;


   procedure Log_Message(Handler: Log_Handler; Info: Log_Message_Type) is abstract;

end;
