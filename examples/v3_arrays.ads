
with V3_Types; use V3_Types;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;


package V3_Arrays is

    -- NOTE dependent on Positive-Count -> make generic by Index_Type ??
    type F64_Arr is array (Positive_Count range <>) of Float_64;
    type F32_Arr is array (Positive_Count range <>) of Float_32;
    type I64_Arr is array (Positive_Count range <>) of Integer_64;
    type I32_Arr is array (Positive_Count range <>) of Integer_32;
    type I16_Arr is array (Positive_Count range <>) of Integer_16;
    type U8_Arr  is array (Positive_Count range <>) of Unsigned_8;

end V3_Arrays;
