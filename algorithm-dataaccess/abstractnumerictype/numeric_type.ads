

-- NOTE for use in FITS_IO it is enough to privide conversions
-- to and from Floats

generic
type T is private;

with function "+"(V : in Float) return T     is <>;
with function "+"(V : in T)     return Float is <>;

package Numeric_Type is

    subtype Numeric is T;

    function Bit_Count return Positive;

    function To_Numeric(V : in Float) return T;
    function To_Float  (V : in T)     return Float;

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

