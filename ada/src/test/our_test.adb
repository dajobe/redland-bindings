package body Our_Test is

   function Get_Environment (C: Our_Test_Case)
                             return Raptor_Environments.Raptor_Environment_Access
   is
      use AUnit.Test_Cases;
   begin
      return Raptor_Environments.Raptor_Environment_Access(AUnit.Test_Cases.Get_Environment(Test_Case(C)));
   end Get_Environment;

end Our_Test;
