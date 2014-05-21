with RDF.Base;

package RDF.Raptor is

   type World is new RDF.Base.Base_Object with null record;

   overriding function Default_Handle(Object: World) return RDF.Base.Dummy_Record_Access;

   overriding procedure Finalize_Handle(Object: World; Handle: RDF.Base.Dummy_Record_Access);

   overriding function From_Handle(Handle: RDF.Base.Dummy_Record_Access) return World with Inline;

end;
