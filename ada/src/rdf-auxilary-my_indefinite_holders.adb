with Ada.Unchecked_Deallocation;

package body RDF.Auxilary.My_Indefinite_Holders is

   procedure Finalize(Object: in out Holder) is
   begin
      Clear(Object);
   end;

   procedure Adjust(Object: in out Holder) is
   begin
      Object.Ptr := new Element_Type'(Object.Ptr.all);
   end;

   function "=" (Left, Right : Holder) return Boolean is
   begin
      return Element(Left) = Element(Right);
   end;

   function To_Holder (New_Item : Element_Type) return Holder is
   begin
      return (Ada.Finalization.Controlled with new Element_Type'(New_Item));
   end;

   function Is_Empty (Container : Holder) return Boolean is
   begin
      return Container.Ptr = null;
   end;

   procedure Clear (Container : in out Holder) is
      procedure Free is new Ada.Unchecked_Deallocation(Element_Type, Element_Access_Type);
   begin
      Free(Container.Ptr);
   end;

   function Element (Container : Holder) return Element_Type is
   begin
      return Container.Ptr.all;
   end;

   function Element_Access (Container : Holder) return Element_Access_Type is
   begin
      return Container.Ptr;
   end;

   procedure Replace_Element (Container : in out Holder; New_Item  : in Element_Type) is
   begin
      Clear(Container);
      Container.Ptr := new Element_Type'(New_Item);
   end;

   procedure Assign (Target : in out Holder; Source : in Holder) is
   begin
      if Target.Ptr /= Source.Ptr then
         if Source.Ptr = null then
            Clear(Target);
         else
            Replace_Element (Target, Element (Source)); -- Efficient?
         end if;
      end if;
   end;

   function Copy (Source : Holder) return Holder is
   begin
      if Is_Empty(Source) then
         return Empty_Holder;
      else
         return To_Holder (Element (Source)); -- Efficient?
      end if;
   end;


   procedure Move (Target : in out Holder; Source : in out Holder) is
   begin
      if Target.Ptr /= Source.Ptr then
         Clear(Target);
         Target.Ptr := Source.Ptr;
         Source.Ptr := null;
      end if;
   end;

end RDF.Auxilary.My_Indefinite_Holders;
