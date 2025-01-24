with AUnit; use AUnit;
with AUnit.Test_Suites;
package AUnit_Suite is
   function Suite return Test_Suites.Access_Test_Suite;
end AUnit_Suite;
