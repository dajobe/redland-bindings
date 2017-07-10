with PR_XXXX_XXX;

package body Sample_Suite is

   Result : aliased Test_Suite;

   Test_Case : aliased PR_XXXX_XXX.Test_Case;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Case'Access);
      return Result'Access;
   end Suite;

end Sample_Suite;
