

with Header;
with Ada.Streams.Stream_IO;

package Data_Value is

   type Access_Rec is -- FIXME also defined in private of FITS_IO.ads
      record
         BITPIX : Integer;
         A,B : Float;
         Undef_Used : Boolean;
         Undef_Raw  : Float;
         Undef_Phys : Float;
      end record;

   type     Count          is new Ada.Streams.Stream_IO.Count;
   subtype  Positive_Count is Count range 1 .. Count'Last;

   subtype String_80 is String(1 .. 80);
   type String_80_Array is array (Positive_Count range <>) of String_80;


   procedure Parse_Image_Cards
      (Image_Cards : in String_80_Array;
      A : out Float;
      B : out Float;
      Undef_Raw_Valid : in out Boolean;
      Undef_Raw_Value : out Float);


   function To_Array_Keys
      (DU_Access : Access_Rec)
      return Header.Valued_Key_Record_Arr;


end Data_Value;
