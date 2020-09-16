

generic
type T is private;
package Numeric_Type is
    type Numeric is new T;
    type T_Arr is array (Positive range <>) of T;
    function Bit_Count return Positive;

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

end Numeric_Type;

