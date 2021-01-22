


with Ada.Streams.Stream_IO;
with FITS_IO; use FITS_IO; -- NAXIS_Array needed
with Mandatory;-- Result_Rec needed
with Ada.Strings.Bounded; use Ada.Strings.Bounded; -- Max20 only FIXME !!
with Optional; use Optional;-- Bounded_String_8_Arr & Card_Arr needed 

package Card is

   function To_Value_String( V : in Integer) return String;
   function To_Value_String( V : in String) return String;
   function To_Value_String( V : in Boolean) return String;

   function Create_Card(Key : in String; Value : in String) return String_80;
   function Create_Mandatory_Card(Key    : in String; Value : in String) return String_80;
   function Create_NAXIS_Card_Arr(NAXISn : in NAXIS_Array) return String_80_Array;


end Card;

