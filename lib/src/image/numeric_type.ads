

-- NOTE for use in FITS_IO it is enough to privide conversions
-- to and from Floats

--with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed
with FITS; use FITS; -- Positive_Count needed

generic
type Numeric is private;
type Numeric_Arr is array (Positive_Count range <>) of Numeric;
type Float_Arr   is array (Positive_Count range <>) of Float;

with function "+"(V : in Float)   return Numeric is <>;
with function "+"(V : in Numeric) return Float   is <>;
with function Is_Undef  (V,U : in Numeric) return Boolean is <>;
with function To_BITPIX (V   : in Numeric) return Integer is <>;

package Numeric_Type is

    function Bit_Count return Positive;
    function BITPIX return Integer;

    procedure Set_Undefined(U : Numeric);
    function  Is_Undefined_Valid return Boolean;
    function  Get_Undefined return Numeric;

    function To_Numeric(V : in Float)   return Numeric;
    function To_Float  (V : in Numeric) return Float;
    -- used in Undefined-value computaion (one value only)

    function To_Numeric(Af : in Float_Arr)   return Numeric_Arr;
    function To_Float  (An : in Numeric_Arr) return Float_Arr;
    -- used in Data Unit values conversion and scaling (array of values)

end Numeric_Type;


-- TODO
    -- for all Ada-types see package Standard: (GNAT implementation)
    -- https://en.wikibooks.org/wiki/Ada_Programming/Libraries/Standard/GNAT

    -- GNAT/Standard basic num-types are:

    -- integers are always 'range' metatype, not 'mod' !!

    -- Short_Short_Integer
    -- Short_Integer
    -- Integer
    -- Long_Integer
    -- Long_Long_Integer

    -- Short_Float
    -- Float
    -- Long_Float
    -- Long_Long_Float

