//
//
// It compiles, runs and does nothing.
//
// If it wasn't my 2nd C# program, I might be able move onwards
//

namespace RedlandTest
{

  using System;
  using System.Runtime.InteropServices;

  public class test
  {

  [DllImport("librdf", EntryPoint="librdf_new_world")]
  public static extern IntPtr librdf_new_world();

  [DllImport("librdf", EntryPoint="librdf_free_world")]
  public static extern void librdf_free_world(IntPtr world);

  [DllImport("librdf", EntryPoint="librdf_world_open")]
  public static extern void librdf_world_open(IntPtr world);

  [DllImport("librdf", EntryPoint="librdf_new_node_from_uri_string")]
  public static extern IntPtr librdf_new_node_from_uri_string(IntPtr world, string uri_string);

  [DllImport("librdf", EntryPoint="librdf_node_to_string")]
  public static extern string librdf_node_to_string(IntPtr node);

    public static void Main(String[] args)
    {
      try {
        Console.Error.WriteLine("Starting");

        IntPtr world = librdf_new_world();
        librdf_world_open(world);

	IntPtr node=librdf_new_node_from_uri_string(world, "http://www.dajobe.org/");
        Console.Error.WriteLine("Node");
        Console.Error.WriteLine(librdf_node_to_string(node));

        librdf_free_world(world);

        Console.Error.WriteLine("Ending");
      } catch (  System.Exception exception) {
        Console.Error.WriteLine(exception);
      }

    }

  }; // end public class


}
