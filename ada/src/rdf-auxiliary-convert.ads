with RDF.Auxiliary.C_Pointers;

package RDF.Auxiliary.Convert is

   function Value_With_Possible_NULs (Item: RDF.Auxiliary.C_Pointers.Pointer; Length: size_t) return String;

end RDF.Auxiliary.Convert;
