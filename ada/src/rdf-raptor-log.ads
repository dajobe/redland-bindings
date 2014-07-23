-- TODO
with Interfaces.C;

package RDF.Raptor.Log is

   type Log_Level_Type is (None, Trace, Debug, Info, Warn, Error, Fatal);
   for Log_Level_Type'Size use Interfaces.C.int'Size; -- hack
   function Last return Log_Level_Type is (Fatal); -- TODO: Use rename instead

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

   type Log_Handler is abstract tagged null record;

   type Log_Message_Type is
      record
         Code: Interfaces.C.int;
         -- TODO
      end record;


   procedure Log_Message(Handler: Log_Handler; Info: Log_Message_Type) is abstract;

end RDF.Raptor.Log;
