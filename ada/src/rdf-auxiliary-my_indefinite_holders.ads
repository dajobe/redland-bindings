-- With Ada.Containers.Indefinite_Holders
-- Reference(Object).Element.all works only for varaible Object.
-- This makes impossible to extract chars_ptr from a constant Object.
-- As such I implement my own holder with API similar to Ada.Containers.Indefinite_Holders.
-- (It is a hack, don't take this code seriously.)
generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package RDF.Auxiliary.My_Indefinite_Holders is

   type Holder is tagged private;

   type Element_Access_Type is access Element_Type;

   Empty_Holder : constant Holder;

   function "=" (Left, Right : Holder) return Boolean;

   not overriding function To_Holder (New_Item : Element_Type) return Holder;

   not overriding function Is_Empty (Container : Holder) return Boolean;

   not overriding procedure Clear (Container : in out Holder);

   not overriding function Element (Container : Holder) return Element_Type;

   not overriding function Element_Access (Container : Holder) return Element_Access_Type;

   not overriding procedure Replace_Element (Container : in out Holder;
                                             New_Item  : in     Element_Type);

   not overriding procedure Assign (Target : in out Holder; Source : in Holder);

   not overriding function Copy (Source : Holder) return Holder;

   not overriding procedure Move (Target : in out Holder; Source : in out Holder);

private

   type Holder is new Ada.Finalization.Controlled with
      record
         Ptr: Element_Access_Type;
      end record;

   overriding procedure Finalize(Object: in out Holder);

   overriding procedure Adjust(Object: in out Holder);

   Empty_Holder : constant Holder := (Ada.Finalization.Controlled with Ptr=>null);

end RDF.Auxiliary.My_Indefinite_Holders;
