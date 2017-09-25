with RDF.Auxiliary;
with RDF.Raptor.World; use RDF.Raptor.World;

package body RDF.Raptor.Options is

   function Get_Domain (Description: Option_Description_Type_Without_Finalize) return Domain_Type is
   begin
      return Get_Handle(Description).Domain;
   end;

   function Get_Option (Description: Option_Description_Type_Without_Finalize) return Raptor_Option is
   begin
      return Get_Handle(Description).Option;
   end;

   function Get_Value_Type (Description: Option_Description_Type_Without_Finalize) return Value_Type_Type is
   begin
      return Get_Handle(Description).Value_Type;
   end;

   function Get_Name (Description: Option_Description_Type_Without_Finalize) return String is
      Handle: constant Option_Description_Handle := Get_Handle(Description);
   begin
      return Value(Handle.Name, Handle.Len);
   end;

   function Get_Label (Description: Option_Description_Type_Without_Finalize) return String is
   begin
      return Value(Get_Handle(Description).Name);
   end;

   function Get_URI (Description: Option_Description_Type_Without_Finalize) return URI_Type_Without_Finalize is
   begin
      return From_Handle(Get_Handle(Description).URI);
   end;

   function raptor_option_get_count return unsigned
     with Import, Convention=>C;

   function Get_Options_Count return Natural is
   begin
      return Natural(raptor_option_get_count);
   end;

   function raptor_option_get_value_type_label (Value_Type: Value_Type_Type) return chars_ptr
     with Import, Convention=>C;

   function Value_Type_Label (Value_Type: Value_Type_Type) return String is
      Ptr: constant chars_ptr := raptor_option_get_value_type_label(Value_Type);
   begin
      if Ptr = Null_Ptr then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Value(Ptr);
   end;

   function raptor_world_get_option_description (World: Raptor_World_Handle; Domain: Domain_Type; Option: Raptor_Option) return Option_Description_Handle
     with Import, Convention=>C;

   function Get_Option_Description (World: Raptor_World_Type_Without_Finalize'Class; Domain: Domain_Type; Option: Natural)
                                    return Option_Description_Type is
   begin
      return Get_Option_Description(World, Domain, Raptor_Option'Val(Option));
   end;

   function Get_Option_Description (World: Raptor_World_Type_Without_Finalize'Class; Domain: Domain_Type; Option: Raptor_Option)
                                    return Option_Description_Type is
   begin
      return From_Non_Null_Handle( raptor_world_get_option_description(Get_Handle(World), Domain, Option) );
   end;

   procedure raptor_world_get_option_description (Descrition: Option_Description_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Option_Description_Type; Handle: Option_Description_Handle) is
   begin
      raptor_world_get_option_description(Handle);
   end;

   function raptor_world_get_option_from_uri (World: Raptor_World_Handle; URI: URI_Handle) return Raptor_Option
     with Import, Convention=>C;

   -- Crude hack
   function Option_From_URI (World: Raptor_World_Type_Without_Finalize'Class; URI: URI_Type'Class) return Raptor_Option is
      Result: constant Raptor_Option'Base := raptor_world_get_option_from_uri(Get_Handle(World), Get_Handle(URI));
   begin
      if not Result'Valid then
         raise Constraint_Error;
      end if;
      return Result;
   end;

end RDF.Raptor.Options;
