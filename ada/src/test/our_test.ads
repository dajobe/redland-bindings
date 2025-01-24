with AUnit.Test_Cases;
with Raptor_Environments;

package Our_Test is

   type Our_Test_Case is abstract new AUnit.Test_Cases.Test_Case with null record;

   function Get_Environment (C: Our_Test_Case)
                             return Raptor_Environments.Raptor_Environment_Access;

end Our_Test;
