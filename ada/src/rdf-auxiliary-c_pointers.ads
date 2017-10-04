with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;

package RDF.Auxiliary.C_Pointers is
  new Interfaces.C.Pointers(Index              => size_t,
                            Element            => char,
                            Element_Array      => char_array,
                            Default_Terminator => nul);
