with AUnit.Test_Cases;
with AUnit.Assertions;
with Ada.Strings.Fixed;
with Interfaces.C;
with RDF.Raptor.Constants; use RDF.Raptor.Constants;

package body Constants_Test is

   subtype unsigned is Interfaces.C.unsigned;
   use type unsigned;

   -- This just creates and assigns some variables (and ignores their values)
   procedure Test_Open(T : in out Test_Cases.Test_Case'Class) is
      function My_Image (I : unsigned) return String is
      begin
         return Ada.Strings.Fixed.Trim (unsigned'Image (I), Ada.Strings.Left);
      end My_Image;
      Combined_String: constant String :=
        My_Image(Raptor_Version_Major) & '.' & My_Image(Raptor_Version_Minor) & '.' & My_Image(Raptor_Version_Release);
      Combined_Decimal: constant unsigned :=
        Raptor_Version_Major * 10000 + Raptor_Version_Minor * 100 + Raptor_Version_Release;
   begin
      AUnit.Assertions.Assert (Combined_String = raptor_version_string, "Combined version string");
      AUnit.Assertions.Assert (Combined_Decimal = raptor_version_decimal, "Combined decimal version");
   end;

   function Name (T : Test_Case)
                  return Test_String is
   begin
      return Format ("Some constant string or integer values");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Open'Access, "Testing version of redlang");
   end Register_Tests;

end Constants_Test;
