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
        My_Image(version_major) & '.' & My_Image(version_minor) & '.' & My_Image(version_release);
      Combined_Decimal: constant unsigned :=
        version_major * 10000 + version_minor * 100 + version_release;
   begin
      AUnit.Assertions.Assert (Combined_String = version_string, "Combined version string");
      AUnit.Assertions.Assert (Combined_Decimal = version_decimal, "Combined decimal version");
   end;

   function Name (T : Test_Case)
                  return Test_String is
   begin
      return Format ("Some constants");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Open'Access, "Testing version of redland");
   end Register_Tests;

end Constants_Test;
