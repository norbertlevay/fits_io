
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO; -- Positive_Count needed

with Raw;
with Optional;

generic
type Tf is private;
type Tf_Arr is array (Positive_Count range <>) of Tf;
with package Tf_Raw is new Raw(Tf,Tf_Arr);
with function To_V3Type(Arg : String) return Tf is <>;
package Raw_BLANK is


    function Find_BLANK
        (Cards : in Optional.Card_Arr; -- Optional.Reserved.Array_Keys
        BLANK : out Tf)   -- BLANK card value converted
        return Boolean;   -- BLANK Card found or not


end Raw_BLANK;
