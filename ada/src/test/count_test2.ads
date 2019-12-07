with AUnit; use AUnit;
with Our_Test;

package Count_Test2 is

   type Test_Case is new Our_Test.Our_Test_Case with null record;

   procedure Register_Tests (T : in out Test_Case);
   --  Register routines to be run

   function Name (T : Test_Case)
                  return Test_String;
   --  Returns name identifying the test case

end Count_Test2;
