with Interfaces.C; use Interfaces.C;
with Ada.Containers.Indefinite_Holders;

package RDF.Auxiliary is

   -- Internal
   type Dummy_Record is null record
     with Convention=>C;

   type Dummy_Record_Access is access Dummy_Record
     with Convention=>C;

   type C_File_Record is null record
     with Convention=>C;

   -- Represents C FILE* record
   type C_File_Access is access C_File_Record
     with Convention=>C;

   type Comparison_Result is range -1..1;

   function Sign (Value: Interfaces.C.int) return Comparison_Result;

   package String_Holders is new Ada.Containers.Indefinite_Holders(String);

   function Length (Object: String_Holders.Holder) return size_t;

   Null_Handle  : exception;
   RDF_Exception: exception; -- all other librdf exceptions

end RDF.Auxiliary;
