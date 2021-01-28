-- TODO:
-- implement Optional for IndexedKeys (arrays)

with Ada.Streams.Stream_IO; --use Ada.Streams.Stream_IO;-- (Positive_)Count needed
with Ada.Strings.Bounded; --	use Ada.Strings.Bounded;
with Keyword_Record; --use Keyword_Record; -- String_80 needed

with FITS; use FITS; -- Count needed
--with FITS_IO; use FITS_IO; -- Count needed

package Optional is

   package SIO renames Ada.Streams.Stream_IO;
   package KWR renames Keyword_Record;

--   package BS  renames Ada.Strings.Bounded;
   package BS_8 renames FITS.BS_8;--is new BS.Generic_Bounded_Length( 8); 
   package BS70 renames FITS.BS70;--is new BS.Generic_Bounded_Length(70);

   subtype Bounded_String_8_Arr is FITS.BS_8_Array;
   --type Bounded_String_8_Arr  is array (Natural range <>) of BS_8.Bounded_String;


subtype Card_Arr is String_80_Array;
--subtype Card_Arr is FITS_IO.String_80_Array;
Null_Card_Arr : Optional.Card_Arr(1 .. 0) := (others => ENDCard);

function Init (Keys : in Bounded_String_8_Arr) return Positive_Count;
function Next (Pos  : in Positive_Count; Card : in KWR.String_80) return Count;
function Get_Cards return Card_Arr;

-- ops on Card_Arr

function Find_Key(Cards : Optional.Card_Arr; Key : BS_8.Bounded_String) return Card_Arr;



-- Valued-key record

--package BS70 is new BS.Generic_Bounded_Length(70);
 subtype Key_Type   is BS_8.Bounded_String;
 subtype Value_Type is BS70.Bounded_String;
 type Valued_Key_Record is
     record
         Key   : Key_Type;
         Value : Value_Type;
     end record;

 type Valued_Key_Record_Arr is array (Natural range <>) of Valued_Key_Record;


 procedure VKR_Read (
            Stream : not null access Ada.Streams.Root_Stream_Type'Class;
            Item   : out  Valued_Key_Record);


 procedure VKR_Write (
            Stream : not null access Ada.Streams.Root_Stream_Type'Class;
            Item   : in  Valued_Key_Record);

 for Valued_Key_Record'Read  use VKR_Read;
 for Valued_Key_Record'Write use VKR_Write;

end Optional;
