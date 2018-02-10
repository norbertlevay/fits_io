--
-- FITS.* packages
--
-- Media management:
--
-- FITS.File child-package: helps to do file management
-- for FITS-files, positioning in FITS-file, list content etc...
-- It buids on Ada.Streams.Stream_IO and so many
-- procedures accept Ada.Stream_IO.File_Type and File_Mode
-- as parameter.
-- Network: ...?
--
-- Data Access:
--
-- FITS package: Once Ada-file Stream is successfully acquired,
-- serialization/deserialization into/from stream
-- happens simply by using Read Write attributes of
-- the predefined types, specifically FitsData_Type'Read and 'Write,
-- which is array of base Ada-types and so supported by
-- Ada GNAT implementation [FITS ?][GNAT ?].
--
-- FIXME Note: currently only PrimaryHeader & Image extension supported.
-- Table Extensions to be added.
-- Parse_Card recognizes only mandatory keywords for primary header which
-- are essentially the same as for IMAGE extension [FITS Sect 7.1, Table 13].
-- Each extension-type has its own set of mandatory keywords.
-- [FITS Sect 4.4.1.2 Conforming Extensions]: Extensions have
-- PCOUNT & GCOUNT keys, but except of BINTABLE (where PCOUNT/=0)
-- their value is 0 & 1 respectively, and so yield (Eq2) the same
-- DU size as Primary haeder (Eq1).
-- PCOUNT & GCOUNT were meant for Random Groups [FITS Section 6]
--

with Interfaces;
with Ada.Streams.Stream_IO;
with System.Storage_Elements; use System.Storage_Elements;

with FitsFloat;


package FITS is

   -- FITS numeric types are prefixed with F...
   --
   -- 1. deriving from file-system representation (Stream_IO):
   -- subtype FNatural  is SIO.Count;
   -- subtype FPositive is SIO.Positive_Count;
   -- FIXME check-out difference; this also possible:
   -- type FPositive is new SIO.Count
   --
   -- 2. deriving from FITS-Standard:
   type    FInteger  is new Long_Long_Integer;
   subtype FNatural  is FInteger range 0 .. FInteger'Last;
   subtype FPositive is FNatural range 1 .. FNatural'Last;
   -- note: package division into FITS and FITS.File
   --       favours 2. (from FITS Standard)

   ------------------
   -- Parse Header --
   ------------------

   CardSize : constant Positive := 80;
   -- [FITS Sects. 3.3.1, 4.4.1]

   subtype Card_Type is String(1..CardSize);
   -- makes sure index start with 1

   ENDCard   : constant Card_Type := ( 1=>'E', 2=>'N', 3=>'D', others => ' ');
   EmptyCard : constant Card_Type := (others => ' ');


   MaxAxes : constant Positive := 999; -- [FITS, Sect 4.4.1]
   subtype NAXIS_Type is Natural range 0 .. MaxAxes;
   -- [FITS 4.4.1.1 Primary Header] "A value of zero signifies
   -- that no data follow the header in the HDU."

   type Dims_Type is array (1..MaxAxes) of FPositive;
   -- [FITS 4.2.3 Integer number]:
   -- FITS poses no limit on max value of Integer / NAXISn.
   -- So max value NAXISn will be implementation limited:
   -- 0 .. FPositive'Last

   type DU_Size_Type is record
      -- Primary HDU:
      BITPIX : Integer;     -- BITPIX from header (data size in bits)
      NAXIS  : NAXIS_Type;  -- NAXIS  from header
      NAXISn : Dims_Type;   -- NAXISn from header, 0 means dimension not in use
      -- Conforming extensions:
      PCOUNT : FNatural;    -- BINTABLE: size of heap OR Random Groups: param count preceding each group
      GCOUNT : FPositive;   -- Number of Random Groups present
      -- FIXME what type to use for P/GCOUNT ? -> implementation limited?
   end record;
   -- collects keyword values which define DataUnit size

   type HDU_Size_Type is record
      XTENSION      : String(1..10); -- XTENSION type string or empty
      CardsCnt      : FPositive;     -- number of cards in this Header (gives Header-size)
      DUSizeKeyVals : DU_Size_Type;  -- keyword values to calc DataUnit-size
   end record;

   procedure Parse_Card (Card          : in Card_Type;
                         DUSizeKeyVals : in out DU_Size_Type);
   -- to be called for every card in Header and will fill-in DUSizeKeyVals's
   -- if all correponding keywords existed in the header
   -- FIXME : what if some needed key missing?

   procedure Parse_Card (Card         : in Card_Type;
                         XtensionType : in out String);

   -----------------------
   -- Size computations --
   -----------------------

   BlockSize_bits : constant FPositive := 2880*8; -- 23040 bits
   -- [FITS 3.1 Overall file structure]

   function  Size_blocks (CardsCnt      : in FPositive   ) return FPositive;
   function  Size_blocks (DUSizeKeyVals : in DU_Size_Type) return FPositive;
   -- size of Header and DataUnit respectively, counted in FITS-blocks

   function  Free_Card_Slots (CardsCnt : in FPositive ) return Natural;
   -- calc number of free cards to fill up HeaderBlock (always 0..35 < Cards/Block)
   procedure Free_Data_Slots (DataCnt :  in FPositive; FreeDataCnt: out Natural) is null;
   -- FIXME add (as function) later when needed: calc number of free data array slots to fill up DataBlock

   ----------------------------------------------
   -- HeaderUnit DataUnit types for Read/Write --
   ----------------------------------------------

   CardsCntInBlock : constant Positive := 36;
   type HeaderBlock_Type is array (1 .. CardsCntInBlock) of Card_Type;

   -- Access Header
   type HBlockArr_Type  is array ( Positive range <> ) of HeaderBlock_Type;
   type CardArr_Type    is array ( Positive range <> ) of Card_Type;
   type CharArr_Type    is array ( Positive range <> ) of Character;

   -- Access DataUnit
   type Int8Arr_Type    is array ( Positive range <> ) of Interfaces.Integer_8;-- <-- Fix this!!
    -- FIXME [FITS Sect 5.2 .. 5.3] says that 8bit is UNSIGNED all others are SIGNED
    -- Note: if unsigned needed for Int16..Int64 BZERO keyword is used to shift the value range
   type Int16Arr_Type   is array ( Positive range <> ) of Interfaces.Integer_16;
   type Int32Arr_Type   is array ( Positive range <> ) of Interfaces.Integer_32;
   type Int64Arr_Type   is array ( Positive range <> ) of Interfaces.Integer_64;
--   type Float32Arr_Type is array ( Positive range <> ) of FitsFloat.FFloat32_BE;
   type Float32Arr_Type is array ( Positive range <> ) of Interfaces.IEEE_Float_32;
   type Float64Arr_Type is array ( Positive range <> ) of Interfaces.IEEE_Float_64;


   -- Endianness
   -- [FITS] Float32/64 are always BigEndian
   -- Write: if machine not BigEndian swap bytes in all Floats of the array
   procedure Float32Arr_Write
              (S :  access Ada.Streams.Root_Stream_Type'Class;
              F32Arr : in Float32Arr_Type);
   for Float32Arr_Type'Write use Float32Arr_Write;
   -- FIXME redefi of Write attrib should on Float_32 type itself; not the array

   procedure Endianness_Float32( F32Arr : in out Float32Arr_Type );
    -- For each Float32 in the array, reverses order of the 4 bytes
    -- within one Float32
    -- Finish Endianness section

   procedure Find_MinMax_Float32
              (F32Arr : in  Float32Arr_Type;
               Min    : out Interfaces.IEEE_Float_32;
               Max    : out Interfaces.IEEE_Float_32);
   -- find minimum and maximum value of the Float32 data array

   type FitsData_Type is
       (HBlock, Card, Char,        -- Header types
        Int8, Int16, Int32,        -- DataUnit types
        Int64, Float32, Float64);
         -- [FITS, Sect 4.4.1.1 Table 8]

   function  To_FitsDataType (BITPIX : in Integer ) return FitsData_Type;

   type DataArray_Type ( FitsType : FitsData_Type ;
                         Length   : Positive ) is
     record
       case FitsType is
       when HBlock =>  HBlockArr  : HBlockArr_Type (1 .. Length);
       when Card  =>   CardArr    : CardArr_Type (1 .. Length);
       when Char  =>   CharArr    : CharArr_Type (1 .. Length);
       when Int8  =>   Int8Arr    : Int8Arr_Type (1 .. Length);
       when Int16 =>   Int16Arr   : Int16Arr_Type(1 .. Length);
       when Int32 =>   Int32Arr   : Int32Arr_Type(1 .. Length);
       when Int64 =>   Int64Arr   : Int64Arr_Type(1 .. Length);
       when Float32 => Float32Arr : Float32Arr_Type(1 .. Length);
       when Float64 => Float64Arr : Float64Arr_Type(1 .. Length);
      end case;
     end record;



   -- in file all data are packed
   pragma Pack (HBlockArr_Type);
   pragma Pack (CardArr_Type);
   pragma Pack (CharArr_Type);

   pragma Pack (Int8Arr_Type);
   pragma Pack (Int16Arr_Type);
   pragma Pack (Int32Arr_Type);
   pragma Pack (Int64Arr_Type);
   pragma Pack (Float32Arr_Type);
   pragma Pack (Float64Arr_Type);
   pragma Pack (DataArray_Type);

end FITS;

