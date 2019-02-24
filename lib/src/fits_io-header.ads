
with Ada.Streams;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;

package FITS_IO.Header is

   type HDU_Type is
       (PrimaryHeader,
        RandomGroups,
        Image,
        AsciiTable,
        BinaryTable);

   subtype Primary   is HDU_Type range PrimaryHeader .. RandomGroups;
   subtype Extension is HDU_Type range Image         .. BinaryTable;

  -- Header cards / key-records decl

   package Max_8 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max =>  8);
   package Max20 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 20);
   package Max48 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 48);
   package Max70 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 70);

   type Key_Record is
    record
        Name    : Max_8.Bounded_String;
        Value   : Max20.Bounded_String;
        Comment : Max48.Bounded_String;
    end record;

   type Comment_Key_Record is
    record
        Name    : Max_8.Bounded_String;
        Comment : Max70.Bounded_String;
    end record;

   type Key_Arr is array (Positive_Count range <>) of Key_Record;

   function To_Key_Record (Card : in Card_Type)
    return Key_Record;

   function Generate_1D_Keys(Name : in String;
                             Min  : in Positive_Count;
                             Max  : in Positive_Count)
    return Key_Arr;
 

   -- types for HDU-Type and HDU-Size parsing

   type NAXIS_Arr is array (NAXIS_Type range <>) of FITS_IO.Count;

   type HDU_Type_Size is
    record
      HDUType  : HDU_Type;
      NAXIS    : NAXIS_Type;
    end record;

   type HDU_Size(NAXIS : NAXIS_Type) is
    record
      -- Primary HDU:
      BITPIX : Integer;
      NAXISn : NAXIS_Arr(1 .. NAXIS);
      -- Conforming extensions:
      PCOUNT : FITS_IO.Count;
      GCOUNT : FITS_IO.Positive_Count;   -- Number of Random Groups present
      -- FIXME what type to use for P/GCOUNT ? -> implementation limited?
    end record;


   -- def key-arrays for parsing of HDU-type and HDU-size

   HDUTypeKeys : Key_Arr := (
    (Max_8.To_Bounded_String("SIMPLE"),   Max20.To_Bounded_String(""), Max48.To_Bounded_String("")),
    (Max_8.To_Bounded_String("GROUPS"),   Max20.To_Bounded_String(""), Max48.To_Bounded_String("")),
    (Max_8.To_Bounded_String("NAXIS1"),   Max20.To_Bounded_String(""), Max48.To_Bounded_String("")),
    (Max_8.To_Bounded_String("XTENSION"), Max20.To_Bounded_String(""), Max48.To_Bounded_String("")), 
    (Max_8.To_Bounded_String("NAXIS"),    Max20.To_Bounded_String(""), Max48.To_Bounded_String("")) );

   function Gen_Size_Keys(NAXIS : in NAXIS_Type)
     return Key_Arr;


   -- conversions

   function To_HDU_Type (Keys : in Key_Arr)
     return HDU_Type_Size;

   function To_HDU_Size (Keys    : in Key_Arr;
                         HDUType : in HDU_Type;
                         NAXIS   : in NAXIS_Type)
     return HDU_Size;


   -- calc size  

   -- FIXME Where to put these ...
   BlockSize_bits : constant FITS_IO.Positive_Count := 2880 * Byte'Size; -- 23040 bits
   StreamElemSize_bits : FITS_IO.Positive_Count := Ada.Streams.Stream_Element'Size;
   BlockSize_bytes : FITS_IO.Positive_Count := BlockSize_bits / StreamElemSize_bits;

   function Size_blocks (HDUSize : in HDU_Size)
     return FITS_IO.Positive_Count;
 
end FITS_IO.Header;
