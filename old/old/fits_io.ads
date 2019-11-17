----------------------------------------------------------------------
-- Ref:                                                             --
--                                                                  --
-- FITS : Definition of the Flexible Image Transport System (FITS)  --
--        The FITS Standard                                         --
--        Version 3.0: approved 2008 July 10 by the IAUFWG          --
--        Document publication date: 2010 November 18               --
--                                                                  --
-- ADA  : Ada Reference Manual, ISO/IEC 8652:2012(E)                --
--        Language and Standard Libraries                           --
--        Copyright @ 2008, 2009, 2010, 2011, 2012 AXE Consultants  --
--                                                                  --
-- GNAT : GNAT Reference Manual                                     --
--        GNAT, The GNU Ada Compiler                                --
--        For gcc version 4.8.2                                     --
--        by AdaCore                                                --
--        Copyright c 1995-2012, Free Software Foundation, Inc.     --
----------------------------------------------------------------------

with Ada.Strings.Bounded;
 -- for Header definition

with Interfaces;
with Ada.Streams.Stream_IO;
 -- both for DataUnit definitions

package FITS_IO is

   type FITS_File_Type is limited private;

   type FITS_File_Mode is (In_File, Inout_File, Out_File, Append_File);

   ---------------------
   -- File Management --
   ---------------------

   procedure Create
     (File : in out FITS_File_Type;
      Mode : in FITS_File_Mode;
      Name : in String;
      Form : in String := "shared=no"); --[GNAT 9.2 FORM strings]

   procedure Open
     (File : in out FITS_File_Type;
      Mode : in FITS_File_Mode;
      Name : in String;
      Form : in String := "shared=no"); --[GNAT 9.2 FORM strings]

   procedure Close (File : in out FITS_File_Type);

   ---------------------------------
   -- Input and Output Operations --
   ---------------------------------

   type Count is new Ada.Streams.Stream_IO.Count;

   subtype Positive_Count is Count range 1 .. Count'Last;

   -- Header definition

   CardSize : constant Positive := 80;
   -- [FITS Sects. 3.3.1, 4.4.1]

   package Card is new Ada.Strings.Bounded.Generic_Bounded_Length(CardSize);
   type Header_Type is array (Positive range <>) of Card.Bounded_String;
   -- Headers stored in text files are lines of max 80 characters

   HDU_AfterLast : constant := Positive'Last;
   -- Write's default behaviour is append a Header after last HDU in file

   function  Read (File : in FITS_File_Type; HDU_Num : in Positive) return Header_Type;

   procedure Write
     (File    : in out FITS_File_Type;
      Header  : in Header_Type;
      HDU_Num : in Positive := HDU_AfterLast); -- default: Append

   ---------------------------------
   -- FITS-file structure (HDU's) --
   ---------------------------------

   type BITPIX_Type is (Int8, Int16, Int32, Int64, Float32, Float64);
   -- [FITS, Sect 4.4.1.1 Table 8]

   for  BITPIX_Type use
     (Int8    =>   8, -- Character or unsigned binary integer
      Int16   =>  16, -- 16-bit two's complement binary integer
      Int32   =>  32, -- 32-bit two's complement binary integer
      Int64   =>  64, -- 64-bit two's complement binary integer
      Float32 => 932, -- IEEE single precision floating point
      Float64 => 964);-- IEEE double precision floating point
      -- FIXME [FITS] defines Floats negative -32 -64

   MaxAxes : constant Positive := 999; -- [FITS, Sect 4.4.1]
   subtype NAXIS_Type is Natural range 0 .. MaxAxes;
   -- [FITS 4.4.1.1 Primary Header] "A value of zero signifies
   -- that no data follow the header in the HDU."

   type Dim_Type is array (1..MaxAxes) of Count;-- FITS poses no limit on max value of NAXISi
   type HDU_Info_Type is record
      CardsCnt : Positive;    -- number of cards in this Header
      Data     : BITPIX_Type; -- data type as given by BITPIX
      BitPix   : Positive;    -- BITPIX from Header (data size in bits)
      Naxes    : NAXIS_Type;  -- NAXIS  from header
      Naxis    : Dim_Type;    -- NAXISi from header, 0 means dimension not in use
   end record;

   Null_HDU_Info : constant HDU_Info_Type := (1,Int32,4,0,(others=>0));

   type HDU_Info_Arr is array (Positive range <>) of HDU_Info_Type;

   procedure List_HDUInfo (File : in FITS_File_Type;
                           Print: not null access
                             procedure(HDUInfo : HDU_Info_Type; Index : Positive));
   -- HDUVect variant

   -----------------
   -- Data access --
   -----------------

   type Int8Arr_Type    is array ( Positive_Count range <> ) of Interfaces.Integer_8;
   type Int16Arr_Type   is array ( Positive_Count range <> ) of Interfaces.Integer_16;
   type Int32Arr_Type   is array ( Positive_Count range <> ) of Interfaces.Integer_32;
   type Int64Arr_Type   is array ( Positive_Count range <> ) of Interfaces.Integer_64;
   type Float32Arr_Type is array ( Positive_Count range <> ) of Interfaces.IEEE_Float_32;
   type Float64Arr_Type is array ( Positive_Count range <> ) of Interfaces.IEEE_Float_64;

   type DataArray_Type ( Option : BITPIX_Type ;
                         Length : Positive_Count ) is
     record
       case Option is
       when Int8  =>   Int8Arr    : Int8Arr_Type (1 .. Length);
       when Int16 =>   Int16Arr   : Int16Arr_Type(1 .. Length);
       when Int32 =>   Int32Arr   : Int32Arr_Type(1 .. Length);
       when Int64 =>   Int64Arr   : Int64Arr_Type(1 .. Length);
       when Float32 => Float32Arr : Float32Arr_Type(1 .. Length);
       when Float64 => Float64Arr : Float64Arr_Type(1 .. Length);
      end case;
     end record;

   function  Read
     (File       : in FITS_File_Type;
      HDU_Num    : in Positive;        -- 1,2,3...
      DataType   : in BITPIX_Type;
      FromOffset : in Positive_Count;  -- in units of DataType
      Length     : in Positive_Count ) -- in units of DataType
    return DataArray_Type;

   procedure Write
     (File     : in FITS_File_Type;
      HDU_Num  : in Positive;         -- 1,2,3...
      ToOffset : in Positive_Count;   -- in units of DataType
      Data     : in DataArray_Type ); -- has length Length(Data)


private

 type FITS_File_Type_Record;
 type FITS_File_Type is access FITS_File_Type_Record;

--   pragma Pack (Int8Arr_Type);
--   pragma Pack (Int16Arr_Type);
--   pragma Pack (Int32Arr_Type);
--   pragma Pack (Int64Arr_Type);
--   pragma Pack (Float32Arr_Type);
--   pragma Pack (Float64Arr_Type);
--   pragma Pack (DataArray_Type);
     -- FIXME seems not to affect Size (see test.adb). Clarify:
     -- Is packing guaranteed by Ada, GNAT or ?

end FITS_IO;
