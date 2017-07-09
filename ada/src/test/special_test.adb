-- https://gcc.gnu.org/bugzilla/show_bug.cgi?id=62236 (FIXED)
with Ada.Iterator_Interfaces;

procedure Special_Test is

   type My_Description_Cursor is null record;

   function Has_Element (Position: My_Description_Cursor) return Boolean is (False);

   package My_Description_Iterators is new Ada.Iterator_Interfaces(My_Description_Cursor, Has_Element);

   type My_Description_Iterator is new My_Description_Iterators.Forward_Iterator with null record;

   overriding function First (Object: My_Description_Iterator) return My_Description_Cursor is (null record);
   overriding function Next (Object: My_Description_Iterator; Position: My_Description_Cursor) return My_Description_Cursor is (null record);

   My_Iterator: My_Description_Iterator;

begin
   for C in My_Iterator loop
      null;
   end loop;
end;
