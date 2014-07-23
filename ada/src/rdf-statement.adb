package body RDF.Statement is

   procedure Handle (Handler: Statement_Handler) is abstract;

   procedure C_Statement_Handler (User_Data: Dummy_Record_Access; ...) is
   begin


end RDF.Statement;
