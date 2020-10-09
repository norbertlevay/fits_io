
-- implements FITSv3 Section 4.2
--
-- 4.2.1 longest string is 68 chars: card(11..80) less the two quotes
-- 4.2.3 4.2.4 ..software packages can limit the range of Integer/float values...

with FITS_IO; use FITS_IO;


package Keyword_Record is

    --package SIO renames Ada.Streams.Stream_IO; -- Count needed

    subtype String_80 is String(1..80);
    ENDCard   : constant String_80 := (1 => 'E', 2 => 'N', 3 => 'D', others => ' ');
    EmptyCard : constant String_80 := (others => ' ');


    function To_Boolean(Value : String) return Boolean;
    function To_Integer(Value : String) return Count; -- FIXME was FInteger
    function To_NAXIS_Index(Value : String) return NAXIS_Index;
    function To_String (Value : String) return String;
    function To_Float (Value : String) return Float;


    function Match_Key(Key : in String; Card : in String_80) return Boolean;
    function Match_Indexed_Key(Root : in String; Card : in String_80) return Boolean;

    function Take_Index(Root : in String; Card : in String_80) return NAXIS_Index;

    Invalid_Card_Value : exception;

end Keyword_Record;


-- not used:
--  function Is_ValuedCard (Card : String_80) return Boolean;

--  function To_String (Value : String) return String;
--  function To_Float  (Value : String) return Float;
--  function To_Complex_Integer(Value : String) return ???;
--  function To_Complex_Float  (Value : String) return ???;

