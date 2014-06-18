package RDF is

   Raptor_Version_Major: constant := 2;
   Raptor_Version_Minor: constant := 0;
   Raptor_Version_Release: constant := 14;

   Raptor_Version: constant := Raptor_Version_Major * 10000 + Raptor_Version_Minor * 100 + Raptor_Version_Release;
   Raptor_Version_String: constant String := Integer'Image(Raptor_Version_Major) & '.' &
   					     Integer'Image(Raptor_Version_Minor) & '.' &
					     Integer'Image(Raptor_Version_Release);

end RDF;
