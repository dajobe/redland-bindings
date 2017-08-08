with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Raptor.World;

package body RDF.Rasqal.Features is

   function rasqal_feature_from_uri (World: RDF.Rasqal.World.Handle_Type; URI: RDF.Raptor.URI.Handle_Type) return Feature_Type
      with Import, Convention=>C;

   function Feature_From_URI (World: World_Type_Without_Finalize'Class; URI: URI_Type_Without_Finalize'Class) return Feature_Type is
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

   function Get_Feature_Description (World: World_Type_Without_Finalize'Class; Feature: Feature_Type) return Feature_Description is
      Name, Label: aliased chars_ptr;
      URI: aliased RDF.Raptor.URI.Handle_Type;
      use String_Holders;
   begin
      if rasqal_features_enumerate(Get_Handle(World), Feature, Name'Unchecked_Access, URI'Unchecked_Access, Label'Unchecked_Access) /= 0
      then
         raise RDF_Exception;
      end if;
      return (Name=>To_Holder(Value(Name)), Label=>To_Holder(Value(Label)), URI=>From_Non_Null_Handle(URI));
   end;

   function rasqal_get_feature_count return unsigned
      with Import, Convention=>C;

   function Get_Feature_Count return unsigned is (Rasqal_Get_Feature_Count);

end RDF.Rasqal.Features;
