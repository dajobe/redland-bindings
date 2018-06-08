--
--  Copyright (C) 2009-2010, AdaCore
--

with AUnit.Simple_Test_Cases;

package AUnit.Test_Suites.Tests_Fixtures is

   --  A very simple minded test case
   type Simple_Test_Case is new AUnit.Simple_Test_Cases.Test_Case with
     null record;
   function Name (Test : Simple_Test_Case) return Message_String;
   procedure Run_Test (Test : in out Simple_Test_Case);

   A_Simple_Test_Case : aliased Simple_Test_Case;

   --  A test case raising a failure
   type TC_With_Failure is new AUnit.Simple_Test_Cases.Test_Case with
     null record;
   function Name (Test : TC_With_Failure) return Message_String;
   procedure Run_Test (Test : in out TC_With_Failure);

   A_TC_With_Failure : aliased TC_With_Failure;

   --  A test case raising two failures
   type TC_With_Two_Failures is new AUnit.Simple_Test_Cases.Test_Case with
     null record;
   function Name (Test : TC_With_Two_Failures) return Message_String;
   procedure Run_Test (Test : in out TC_With_Two_Failures);

   A_TC_With_Two_Failures : aliased TC_With_Two_Failures;

   --  A test case raising an exception
   type TC_With_Exception is new AUnit.Simple_Test_Cases.Test_Case with
     null record;
   function Name (Test : TC_With_Exception) return Message_String;
   procedure Run_Test (Test : in out TC_With_Exception);

   A_TC_With_Exception : aliased TC_With_Exception;

   --  A test case using set_up and tear_down
   type TC_With_Setup is new AUnit.Simple_Test_Cases.Test_Case with record
      Setup : Boolean := False;
      Error : Boolean := False;
   end record;
   function Name (Test : TC_With_Setup) return Message_String;
   procedure Set_Up (Test : in out TC_With_Setup);
   procedure Tear_Down (Test : in out TC_With_Setup);
   procedure Run_Test (Test : in out TC_With_Setup);
   A_TC_With_Setup : aliased TC_With_Setup;

   My_Exception : exception;

end AUnit.Test_Suites.Tests_Fixtures;
