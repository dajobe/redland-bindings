with AUnit.Test_Cases;
with AUnit.Assertions;
with Ada.Strings.Fixed;
with Interfaces.C;
with RDF.Raptor.Constants;
with RDF.Rasqal.Constants;

package body Constants_Test is

   subtype unsigned is Interfaces.C.unsigned;
   use type unsigned;

   function My_Image (I : Unsigned) return String is
   begin
      return Ada.Strings.Fixed.Trim (Unsigned'Image (I), Ada.Strings.Left);
   end My_Image;

   -- This just creates and assigns some variables (and ignores their values)

   procedure Test_Raptor(T : in out Test_Cases.Test_Case'Class) is
      use RDF.Raptor.Constants;
      Combined_String: constant String :=
        My_Image(version_major) & '.' & My_Image(version_minor) & '.' & My_Image(version_release);
      Combined_Decimal: constant unsigned :=
        version_major * 10000 + version_minor * 100 + version_release;
   begin
      AUnit.Assertions.Assert (Combined_String = version_string, "Combined version string");
      AUnit.Assertions.Assert (Combined_Decimal = version_decimal, "Combined decimal version");
   end;

   procedure Test_Rasqal(T : in out Test_Cases.Test_Case'Class) is
      use RDF.Rasqal.Constants;
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
      Register_Routine (T, Test_Raptor'Access, "Testing version of Raptor");
      Register_Routine (T, Test_Rasqal'Access, "Testing version of Rasqal");
   end Register_Tests;

end Constants_Test;
