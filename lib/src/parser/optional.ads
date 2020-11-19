-- TODO:
-- implement Optional for IndexedKeys (arrays)

with Ada.Streams.Stream_IO; --use Ada.Streams.Stream_IO;-- (Positive_)Count needed
with Ada.Strings.Bounded; --	use Ada.Strings.Bounded;
with Keyword_Record; --use Keyword_Record; -- String_80 needed

with FITS_IO; use FITS_IO; -- Count needed

package Optional is

package SIO renames Ada.Streams.Stream_IO;
--package BS  renames Ada.Strings.Bounded;

package KWR renames Keyword_Record;

--package Bounded_String_8 is new BS.Generic_Bounded_Length(8);
--package BS_8 renames Bounded_String_8;
   package BS  renames Ada.Strings.Bounded;

   package BS_8 is new BS.Generic_Bounded_Length( 8); 
   package BS70 is new BS.Generic_Bounded_Length(70);

--   Null_Undefined_Value : constant BS70.Bounded_String
--                                 := BS70.To_Bounded_String("");

type Bounded_String_8_Arr  is array (Natural range <>) of BS_8.Bounded_String;


type Card_Arr is array (Positive range <>) of KWR.String_80;
-- FIXME consider Index of Card_Arr derived from NAXIS value type (SIO.Positive)
-- theory in can be as big as DataUnit-length/80 
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


 procedure VKR_Read (
            Stream : not null access Ada.Streams.Root_Stream_Type'Class;
            Item   : out  Valued_Key_Record);


 procedure VKR_Write (
            Stream : not null access Ada.Streams.Root_Stream_Type'Class;
            Item   : in  Valued_Key_Record);

 for Valued_Key_Record'Read  use VKR_Read;
 for Valued_Key_Record'Write use VKR_Write;

end Optional;
