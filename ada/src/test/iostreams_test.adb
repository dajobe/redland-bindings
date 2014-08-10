with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with AUnit.Test_Cases;
with AUnit.Assertions; use AUnit.Assertions;
with RDF.Raptor.World;
with RDF.Raptor.Iostream; use RDF.Raptor.Iostream;
with Ada.Text_IO;

package body Iostreams_Test is

   procedure Test_Sinks(T : in out Test_Cases.Test_Case'Class) is
      World: RDF.Raptor.World.World_Type;
      Str: aliased char_array := "qqq";
      In_Sink:  Stream_Type := From_Sink (World);
      Out_Sink: Stream_Type := To_Sink (World);
   begin
      Assert (Read_Bytes (To_Chars_Ptr (Str'Unchecked_Access), 10, 10, In_Sink) = 0, "Read zero bytes from a sink");
      Write ("XYZ", Out_Sink); -- does nothing
   end;

   procedure Test_Strings(T : in out Test_Cases.Test_Case'Class) is
      World: RDF.Raptor.World.World_Type;
      Str  : String := "xqqq";
      Buf: aliased char_array := (1..99=>'w', 100=>NUL);
      In_String: Stream_From_String := Open_From_String (World, Str);
      Out_String: Stream_To_String := Open (World);
      Out_String2: Stream_To_String := Open (World);
      Bytes_Read: size_t;
   begin
      Bytes_Read := Read_Bytes (To_Chars_Ptr (Buf'Unchecked_Access), 1, 100, In_String);
      Assert (Bytes_Read = 4, "Read 4 bytes from string");
      Assert (To_Ada (Buf(1..4), Trim_Nul=>False) = Str, "Compare read string");
      Write(Str, Out_String);
      Write("QQ", Out_String);
      Assert (Value (Out_String) = Str & "QQ", "Compare written string");
      Assert (Tell (Out_String) = 4+2, "'Tell' position");
      Decimal_Write (1234, Out_String2);
      Assert (Value (Out_String2) = "1234", "Decimal write");
   end;

   function Name (T : Test_Case)
                  return Test_String is
   begin
      return Format ("Streams");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Sinks'Access, "Testing sinks"); -- FIXME: uncomment
      Register_Routine (T, Test_Strings'Access, "Testing string streams");
   end Register_Tests;

end Iostreams_Test;
