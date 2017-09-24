with Ada.Iterator_Interfaces;
with Interfaces.C; use Interfaces.C;
with RDF.Rasqal.World; use RDF.Rasqal.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;

package RDF.Rasqal.Features is

   type Feature_Type is (Unknown, No_Net, Rand_Seed)
      with Convention=>C;
   for Feature_Type use (Unknown=>-1, No_Net=>0, Rand_Seed=>1);

   function Feature_From_URI (World: Raptor_World_Type_Without_Finalize'Class; URI: URI_Type_Without_Finalize'Class) return Feature_Type;

   type Feature_Value_Type is (Other, Integer_Type, String_Type)
      with Convention=>C;
   for Feature_Value_Type use (Other=>-1, Integer_Type=>0, String_Type=>1);

   function Get_Type (Feature: Feature_Type) return Feature_Value_Type;

   type Feature_Description (Name_Length, Label_Length: Natural) is
      record
         Name: String(1..Name_Length);
         Label: String(1..Label_Length);
         URI: URI_Type; -- with finalize
      end record;

   -- For API simplicity, I don't support the C library feature to retrieve only a part of the data.
   -- For API simplicity, we do not differentiate between failure and unknown feature.
   function Get_Feature_Description (World: Raptor_World_Type_Without_Finalize'Class; Feature: Feature_Type) return Feature_Description;

   function Get_Feature_Count return unsigned;

   type Features_Cursor is private;

   function Get_Position (Cursor: Features_Cursor) return Feature_Type;

   function Get_Description (Cursor: Features_Cursor) return Feature_Description;

   function Has_Element (Position: Features_Cursor) return Boolean;

   package Features_Iterators is new Ada.Iterator_Interfaces(Features_Cursor, Has_Element);

   type Features_Iterator is new Features_Iterators.Forward_Iterator with private;

   overriding function First (Object: Features_Iterator) return Features_Cursor;
   overriding function Next (Object: Features_Iterator; Position: Features_Cursor) return Features_Cursor;

   not overriding function Create_Features_Descriptions_Iterator(World: RDF.Rasqal.World.Raptor_World_Type_Without_Finalize'Class)
                                                                 return Features_Iterator;

private

   type Features_Cursor is
      record
         World: RDF.Rasqal.World.Handle_Type;
         Position: Feature_Type;
      end record;

   type Features_Iterator is new Features_Iterators.Forward_Iterator with
      record
         World: RDF.Rasqal.World.Handle_Type;
      end record;

end RDF.Rasqal.Features;
