with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary;
with RDF.Raptor.World;

package body RDF.Rasqal.Features is

   function rasqal_feature_from_uri (World: RDF.Rasqal.World.Handle_Type; URI: RDF.Raptor.URI.Handle_Type) return Feature_Type
      with Import, Convention=>C;

   function Feature_From_URI (World: Rasqal_World_Type_Without_Finalize'Class; URI: URI_Type_Without_Finalize'Class) return Feature_Type is
      Value: constant Feature_Type := rasqal_feature_from_uri(Get_Handle(World), Get_Handle(URI));
   begin
      return (if Value'Valid then Value else Unknown);
   end;

   function rasqal_feature_value_type (Feature: Feature_Type) return int
      with Import, Convention=>C;

   function Get_Type (Feature: Feature_Type) return Feature_Value_Type is
      Value_Type: constant int := rasqal_feature_value_type(Feature);
   begin
      if Value_Type = 0 then
         return Integer_Type;
      elsif Value_Type = 1 then
         return String_Type;
      else
         return Other;
      end if;
   end;

   type String_P_Type is access all chars_ptr with Convention=>C;
   type URI_P_Type is access all RDF.Raptor.URI.Handle_Type with Convention=>C;

   function rasqal_features_enumerate (World: RDF.Raptor.World.Handle_Type;
                                       Feature: Feature_Type;
                                       Name: String_P_Type;
                                       URI: URI_P_Type;
                                       Label: String_P_Type)
                                       return Int
      with Import, Convention=>C;

   function Get_Feature_Description (World: Rasqal_World_Type_Without_Finalize'Class; Feature: Feature_Type) return Feature_Description is
      Name, Label: aliased chars_ptr;
      URI: aliased RDF.Raptor.URI.Handle_Type;
   begin
      if rasqal_features_enumerate(Get_Handle(World), Feature, Name'Unchecked_Access, URI'Unchecked_Access, Label'Unchecked_Access) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      declare
         Name2 : constant String := Value(Name );
         Label2: constant String := Value(Label);
      begin
         return (Name_Length  => Name2'Length,
                 Label_Length => Label2'Length,
                 Name  => Name2,
                 Label => Label2,
                 URI => From_Non_Null_Handle(URI));
      end;
   end;

   function rasqal_get_feature_count return unsigned
      with Import, Convention=>C;

   function Get_Feature_Count return unsigned is (Rasqal_Get_Feature_Count);

   type C_Features_Enum is range -(2**(Feature_Type'Size-1)) .. (2**(Feature_Type'Size-1))-1
     with Size => Feature_Type'Size, Convention => C;

   function Conv is new Ada.Unchecked_Conversion(C_Features_Enum, Feature_Type);
   function Conv is new Ada.Unchecked_Conversion(Feature_Type, C_Features_Enum);

   function Get_Position (Cursor: Features_Cursor) return Feature_Type is (Cursor.Position);

   function Get_Description (Cursor: Features_Cursor) return Feature_Description is
   begin
      return Get_Feature_Description(Rasqal_World_Type_Without_Finalize'(From_Handle(Cursor.World)), Cursor.Position);
   end;

   function Has_Element (Position: Features_Cursor) return Boolean is
   begin
      return Conv(Position.Position) < C_Features_Enum(Get_Feature_Count);
   end;

   function First (Object: Features_Iterator) return Features_Cursor is
   begin
      return Features_Cursor'(World=>Object.World, Position=>Conv(0));
   end;

   function Next (Object: Features_Iterator; Position: Features_Cursor) return Features_Cursor is
   begin
      return (World=>Object.World, Position=>Conv(Conv(Position.Position) + 1));
   end;

   function Create_Features_Descriptions_Iterator(World: RDF.Rasqal.World.Rasqal_World_Type_Without_Finalize'Class)
                                                  return Features_Iterator is
   begin
      return Features_Iterator'(World => Get_Handle(World));
   end;

end RDF.Rasqal.Features;
