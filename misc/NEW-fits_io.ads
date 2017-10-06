--
-- NEW-fits_io.ads :
-- somewhat lower-level interface (for example no direct Header write etc).
-- When accessing FITS-file, this module handles conversion from packed data-array
-- inside the FITS-file into memory image (possibly not packed but aligned to boundary).
-- E.g. cycles through each array value - at this occasion could perform also data-value
-- scaling (see [FITS??] keywords: BSCALE, BZERO) if requested, and return scaled values.
-- Here, should also handle Endianness (before scaling).
--
-- Before actual Header/Data access, the client has to set HDU_Number explicitely.
-- Then client may (but must not) call Get() to query info about the selected
-- HDU. Such info can be used to define Data_Array of suitable size to read-in all
-- Header/Data, or some portion of it.
--
-- Header & Data are accessed as array of elementar type with the same
-- Read/Write calls. If Data_Type = Char calls access HeaderUnit otherwise
-- calls access DataUnit.
--



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

with Interfaces;
-- BITPIX data sizes:
--with Ada.Streams.Stream_IO;
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

   ------------------------
   -- Positioning by HDU --
   ------------------------

   function  HDU_Number     (File : in FITS_File_Type) return Positive;

   procedure Set_HDU_Number (File : in FITS_File_Type; HDU_Number : in Positive);
   -- HDU_Number is 1,2,3... [FITS ??]

   --------------
   -- HDU info --
   --------------

   MaxAxesCount : constant Positive := 999; -- [FITS, Sect 4.4.1]
   subtype NAXES_Type is Natural range 0 .. MaxAxesCount;
   -- [FITS 4.4.1.1 Primary Header] "A value of zero signifies
   -- that no data follow the header in the HDU."
   type NAXIS_Array is array (1..MaxAxesCount) of Positive;
   -- FITS poses no limit on max value of NAXISi (Positive'Last)

   type Data_Type is (Char, Int8, Int16, Int32, Int64, Float32, Float64);
   -- [FITS, Sect 4.4.1.1 Table 8] and Header as Character Latin2 ? ASCII [FITS ??]

   type HDU_Info_Type is record
      CardsCnt : Positive;    -- number of cards in this Header
      Data     : Data_Type;   -- data type as given by BITPIX + the Header character def
      BitPix   : Positive;    -- BITPIX from Header (data size in bits)
      Naxes    : NAXES_Type;  -- NAXIS  from Header = 1..999
      Naxis    : NAXIS_Array; -- NAXISi array from header, 0 means dimension not in use <--[FITS ref?]
   end record;
   Null_HDU_Info : constant HDU_Info_Type := (1,Int32,4,0,(others=>0));

   function  Get (File : in FITS_File_Type) return HDU_Info_Type;

   -----------------
   -- Data Access --
   -----------------

   type Count is new Ada.Streams.Stream_IO.Count; -- ??
   subtype Positive_Count is Count range 1 .. Count'Last; -- ?? diff Positive vs Positive_Count ?

   type CharArr_Type    is array ( Positive_Count range <> ) of Character;
   -- for Header Unit
   type Int8Arr_Type    is array ( Positive_Count range <> ) of Interfaces.Integer_8;
   type Int16Arr_Type   is array ( Positive_Count range <> ) of Interfaces.Integer_16;
   type Int32Arr_Type   is array ( Positive_Count range <> ) of Interfaces.Integer_32;
   type Int64Arr_Type   is array ( Positive_Count range <> ) of Interfaces.Integer_64;
   type Float32Arr_Type is array ( Positive_Count range <> ) of Interfaces.IEEE_Float_32;
   type Float64Arr_Type is array ( Positive_Count range <> ) of Interfaces.IEEE_Float_64;
   -- for Data Unit

   type Data_Array ( Option : Data_Type ;
                     Length : Positive_Count ) is
     record
       case Option is
       when Char    => CharArr    : CharArr_Type (1 .. Length);-- Header
       when Int8    => Int8Arr    : Int8Arr_Type (1 .. Length);-- Data all other
       when Int16   => Int16Arr   : Int16Arr_Type(1 .. Length);
       when Int32   => Int32Arr   : Int32Arr_Type(1 .. Length);
       when Int64   => Int64Arr   : Int64Arr_Type(1 .. Length);
       when Float32 => Float32Arr : Float32Arr_Type(1 .. Length);
       when Float64 => Float64Arr : Float64Arr_Type(1 .. Length);
      end case;
     end record;

   -- Read Data'Length elements from HDU_Number and Offset in HDU.
   -- Offset is counted from begining of Header- or Data-Unit depending on Data_Type

   procedure Read
     (File   : in FITS_File_Type;
      Data   : in out Data_Array; -- read Length(Data) elements
      Offset : in Natural := 0);  -- in units of Data_Type

   procedure Write
     (File   : in out FITS_File_Type;
      Data   : in Data_Array;     -- write Length(Data) elements
      Offset : in Natural := 0);  -- in units of Data_Type

   -- when Data'Option is Char Read/Write access Header-Unit otherwise Data-Unit

private

 type FITS_File_Type_Record;
 type FITS_File_Type is access FITS_File_Type_Record;

 -- FIXME: we should have no pragma Pack here.
 --        These arrays should be memory-images already.
 --        Packed, in-file versions should be at Block_IO level.
 -- pragma Pack (CharArr_Type);
 -- pragma Pack (Int8Arr_Type);
 -- pragma Pack (Int16Arr_Type);
 -- pragma Pack (Int32Arr_Type);
 -- pragma Pack (Int64Arr_Type);
 -- pragma Pack (Float32Arr_Type);
 -- pragma Pack (Float64Arr_Type);
 -- pragma Pack (DataArray_Type);
 -- FIXME seems not to affect Size (see test.adb). Clarify:
 -- Is packing guaranteed by Ada, GNAT or ?

end FITS_IO;
