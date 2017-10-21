with Interfaces.C; use Interfaces.C;

package body RDF.Redland.Query_Results is

   function librdf_query_results_as_stream (Results: Query_Results_Handle) return Stream_Handle
     with Import, Convention=>C;

   function As_Stream (Results: Query_Results_Type_Without_Finalize) return Stream_Type is
   begin
      return From_Non_Null_Handle(librdf_query_results_as_stream(Get_Handle(Results)));
   end;

   function librdf_query_results_get_count (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   function Get_Current_Count (Results: Query_Results_Type_Without_Finalize) return Natural is
   begin
      return Natural(librdf_query_results_get_count(Get_Handle(Results)));
   end;

   function librdf_query_results_next (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   procedure Next (Results: Query_Results_Type_Without_Finalize'Class) is
   begin
      if librdf_query_results_next(Get_Handle(Results)) /= 0 then
         -- Check is done by Finished procedure, not here
         null; -- raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_query_results_finished (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   function Finished (Results: Query_Results_Type_Without_Finalize) return Boolean is
     (librdf_query_results_finished(Get_Handle(Results)) /= 0);

end RDF.Redland.Query_Results;
