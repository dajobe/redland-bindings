//
//
// It compiles, runs and does nothing.
//
// If it wasn't my 2nd C# program, I might be able move onwards
//

namespace RedlandTest
{

//  using RedlandPINVOKE;
  using System;
  using System.Runtime.InteropServices;

  public class test
  {

    public static void Main(String[] args)
    {
      try {
        Console.Error.WriteLine("Starting");

//        IntPtr world = RedlandPINVOKE.librdf_new_world();
//        RedlandPINVOKE.librdf_world_open(world);

//        RedlandPINVOKE.librdf_free_world(world);

        Console.Error.WriteLine("Ending");
      } catch (  System.Exception exception) {
        Console.Error.WriteLine(exception);
      }

    }

  }; // end public class


}
