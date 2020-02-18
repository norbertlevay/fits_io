
-- implements FITSv3 Section 4.2
--
-- 4.2.1 longest string is 68 chars: card(11..80) less the two quotes
-- 4.2.3 4.2.4 ..software packages can limit the range of Integer/float values...

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO; -- Count needed


package Keyword_Record is

    package SIO renames Ada.Streams.Stream_IO; -- Count needed

    subtype String_80 is String(1..80);
    ENDCard   : constant String_80 := (1 => 'E', 2 => 'N', 3 => 'D', others => ' ');
    EmptyCard : constant String_80 := (others => ' ');


    subtype FIndex is Integer range 0 .. 999;-- FIXME rename and move? elsewhere??
    
    function To_Boolean(Value : String) return Boolean;
    function To_Integer(Value : String) return SIO.Count; -- FIXME was FInteger
    function To_FIndex(Value : String) return FIndex;
    function To_String (Value : String) return String;
    function To_Float (Value : String) return Float;
    

    function Match_Key(Key : in String; Card : in String_80) return Boolean;
    function Match_Indexed_Key(Root : in String; Card : in String_80) return Boolean;

    function Take_Index(Root : in String; Card : in String_80) return FIndex;

    Invalid_Card_Value : exception;

end Keyword_Record;


-- not used:
--  function Is_ValuedCard (Card : String_80) return Boolean;

--  function To_String (Value : String) return String;
--  function To_Float  (Value : String) return Float;
--  function To_Complex_Integer(Value : String) return ???;
--  function To_Complex_Float  (Value : String) return ???;

