--
--  Copyright (C) 2009-2013, AdaCore
--

with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Filters;   use AUnit.Test_Filters;

with AUnit_Suite; use AUnit_Suite;

procedure AUnit_Harness is

   procedure Harness is new AUnit.Run.Test_Runner (Suite);
   --  The full test harness

   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Filter   : aliased AUnit.Test_Filters.Name_Filter;
   Options  : AUnit.Options.AUnit_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => True,
      Report_Successes => True,
      Filter           => null);
begin
   AUnit.Reporter.Text.Set_Use_ANSI_Colors (Reporter, True);
   Harness (Reporter, Options);

   --  Test the filter
   --  This filter should be initialized from the command line arguments. In
   --  this example, we don't do it to support limited runtimes with no support
   --  for Ada.Command_Line

   Options.Filter := Filter'Unchecked_Access;
   Set_Name (Filter, "(test_case) Test routines registration");
   Harness (Reporter, Options);

end AUnit_Harness;
