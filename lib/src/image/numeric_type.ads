

-- NOTE for use in FITS_IO it is enough to privide conversions
-- to and from Floats

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed


generic
type T is private;

with function "+"(V : in Float) return T     is <>;
with function "+"(V : in T)     return Float is <>;

with function To_BITPIX(V : in T) return Integer is <>;

package Numeric_Type is

    subtype Numeric is T;

    type Numeric_Arr is array (Positive_Count range <>) of Numeric;
    type Float_Arr   is array (Positive_Count range <>) of Float;



    function Bit_Count return Positive;
    function BITPIX return Integer;

    function To_Numeric(V : in Float)   return Numeric;
    function To_Float  (V : in Numeric) return Float;

    function To_Numeric(Af : in Float_Arr)   return Numeric_Arr;
    function To_Float  (An : in Numeric_Arr) return Float_Arr;


end Numeric_Type;






-- TODO
    -- function To_Numeric(V : in Integer) return Numeric;
    -- function To_Numeric(V : in Float) return Numeric;
    -- ...
    -- for all Ada-types see package Standard: (GNAT implementation)
    -- https://en.wikibooks.org/wiki/Ada_Programming/Libraries/Standard/GNAT
    -- function To_Integer(V : in Numeric) return Integer;
    -- ...

    -- GNAT/Standard basic num-types are:

    -- integers are always 'range' metytype, not 'mod' !!

    -- Short_Short_Integer
    -- Short_Integer
    -- Integer
    -- Long_Integer
    -- Long_Long_Integer

    -- Short_Float
    -- Float
    -- Long_Float
    -- Long_Long_Float

