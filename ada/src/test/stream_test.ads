with AUnit; use AUnit;
with Our_Test;

package Stream_Test is

   type Test_Case is new Our_Test.Our_Test_Case with null record;

   procedure Register_Tests (T : in out Test_Case);
   --  Register routines to be run

   function Name (T : Test_Case)
                  return Test_String;
   --  Returns name identifying the test case

end Stream_Test;
