with Raptor_Test_Suite;
with AUnit.Run;
with AUnit.Reporter.Text;

procedure Run_All_Tests is
   procedure Run is new AUnit.Run.Test_Runner (Raptor_Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Run_All_Tests;
