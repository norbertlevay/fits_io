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

with Interfaces;


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

   type FitsData_Type is
       (HBlock, Card, Char,        -- Header types
        Int8, Int16, Int32,        -- DataUnit types
        Int64, Float32, Float64);
         -- [FITS, Sect 4.4.1.1 Table 8]

   function  To_FitsDataType (BITPIX : in Integer )
     return FitsData_Type;
   -- FIXME needed when accessing DataUnit-data in general, for whatever type is given in Header
   -- see testfits :: printing some data from a fits-file of whatever
   -- data-type as spec'd in Header -> needed to init variable record DataArray_Type(type,length)

   CardSize : constant Positive := 80;
   -- [FITS Sects. 3.3.1, 4.4.1]

   subtype Card_Type is String(1..CardSize);
   -- makes sure index start with 1

   ENDCard    : constant Card_Type := "END                                                                             ";
   EmptyCard  : constant Card_Type := (others => ' ');

   BlockSize_bits  : FPositive := 2880*8;
   -- FIXME [FITS ??]

   -----------
   -- Sizes --
   -----------

   MaxAxes : constant Positive := 999; -- [FITS, Sect 4.4.1]
   subtype NAXIS_Type is Natural range 0 .. MaxAxes;
   -- [FITS 4.4.1.1 Primary Header] "A value of zero signifies
   -- that no data follow the header in the HDU."

   type Dim_Type is array (1..MaxAxes) of FPositive;
   -- FITS poses no limit on max value of NAXISi
   -- except the space in Card: 11..30 columns:
   -- 19 decimal digits (called by FITS 'fixed integer')
   -- That is slightly over Long_Long_Integer'Last ~ 9.2 x 10**19
   -- vs 19 digits of 9: 9.9999..x10**19.
   -- So max value NAXISn will be implementation limited.
   -- If derived from Stream_IO.Count: which is derived from Address_Size:
   --  e.g. NAXISn will be 32bit or 64bit depending on the machine
   -- FIXME If derived from Long_Long_Integer - see [GNAT??]

   type DUSizeParam_Type is record
      Data   : FitsData_Type; -- data type as given by BITPIX
      BITPIX : Integer;       -- BITPIX from Header (data size in bits)
      Naxes  : NAXIS_Type;    -- NAXIS  from header
      Naxis  : Dim_Type;      -- NAXISi from header, 0 means dimension not in use
   end record;
   -- collects data which defines DataUnit size

   type HDU_Size_Type is record
      CardsCnt    : Positive;    -- number of cards in this Header
      DUSizeParam : DUSizeParam_Type; -- data type as given by BITPIX
   end record;

   procedure Parse_Card (Card        : in Card_Type;
                         DUSizeParam : in out DUSizeParam_Type);

   function  Size_blocks (DUSizeParam : in DUSizeParam_Type) return FPositive;

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
   type Int8Arr_Type    is array ( Positive range <> ) of Interfaces.Integer_8;
   type Int16Arr_Type   is array ( Positive range <> ) of Interfaces.Integer_16;
   type Int32Arr_Type   is array ( Positive range <> ) of Interfaces.Integer_32;
   type Int64Arr_Type   is array ( Positive range <> ) of Interfaces.Integer_64;
   type Float32Arr_Type is array ( Positive range <> ) of Interfaces.IEEE_Float_32;
   type Float64Arr_Type is array ( Positive range <> ) of Interfaces.IEEE_Float_64;


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

