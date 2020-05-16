
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO; -- Positive_Count needed

with Optional;
with Raw;


--generic
--type Tf is private;
--type Tf_Arr is array (Positive_Count range <>) of Tf;
--with function To_V3Type(Arg : String) return Tf is <>;
--with package Tf_Raw is new Raw(Tf);
package body Raw_BLANK is

    function Find_BLANK
        (Cards : in Optional.Card_Arr; -- Optional.Reserved.Array_Keys
        BLANK : out Tf)   -- BLANK card value converted
        return Boolean    -- BLANK Card found or not
    is
        Found : Boolean := False;
    begin
        for I in Cards'Range
        loop
            BLANK := To_V3Type(Cards(I)(11..30));-- FIXME explicit range
        end loop;
        return Found;
    end Find_BLANK;

end Raw_BLANK;
