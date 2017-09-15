--
-- This package serializes/deserializes FITS-Header
-- according to FITS standard, so it is ready for File-streams (Ada.Streams.Stream_IO)
-- or other stream-media.
--
-- Users of this package should do stream-media management:
-- see Stream_IO for files, or some other package for network media.
-- Once Stream is successfully acquired, this package can be used to
-- serialize and place FITS-Header into the stream and/or
-- to get it ("deserialize") from the stream.
--
-- Set_Index allows positioning in the stream if the media allows it
-- (for files yes, for network maybe?).

-- type Header_Rec is new Ada.Streams.Stream_Type ??
-- needs to derive from Stream - probably not
--

with Interfaces;

with Ada.Streams.Stream_IO;

package FITSStream is

   ------------------------------
   -- Positioning in FITS-file --
   ------------------------------

   type Unit_Type is (HeaderUnit, DataUnit);

   procedure Set_Index(FitsFile : in Ada.Streams.Stream_IO.File_Type;
                       HDUNum   : in Positive;     -- which HDU
                       UnitType : in Unit_Type;    -- position to start of HeaderUnit or DataUnit
                       Offset   : in Natural := 0); -- offset within the Unit (in units of FITSData_Type)
   -- set file-index to correct position for Read/Write


   -------------------------------
   -- HeaderUnit DataUnit types --
   -------------------------------

   type CharArr_Type    is array ( Positive range <> ) of Character;
   -- FIXME make sure Ada Character type is of same size as FITS Standard header-character
   type Int8Arr_Type    is array ( Positive range <> ) of Interfaces.Integer_8;
   type Int16Arr_Type   is array ( Positive range <> ) of Interfaces.Integer_16;
   type Int32Arr_Type   is array ( Positive range <> ) of Interfaces.Integer_32;
   type Int64Arr_Type   is array ( Positive range <> ) of Interfaces.Integer_64;
   type Float32Arr_Type is array ( Positive range <> ) of Interfaces.IEEE_Float_32;
   type Float64Arr_Type is array ( Positive range <> ) of Interfaces.IEEE_Float_64;

   type FITSData_Type is (Char, Int8, Int16, Int32, Int64, Float32, Float64);
   -- [FITS, Sect 4.4.1.1 Table 8]

   type DataArray_Type ( Option : FITSData_Type ;
                         Length : Positive ) is
     record
       case Option is
       when Char  =>   CharArr    : CharArr_Type (1 .. Length);
       when Int8  =>   Int8Arr    : Int8Arr_Type (1 .. Length);
       when Int16 =>   Int16Arr   : Int16Arr_Type(1 .. Length);
       when Int32 =>   Int32Arr   : Int32Arr_Type(1 .. Length);
       when Int64 =>   Int64Arr   : Int64Arr_Type(1 .. Length);
       when Float32 => Float32Arr : Float32Arr_Type(1 .. Length);
       when Float64 => Float64Arr : Float64Arr_Type(1 .. Length);
      end case;
     end record;
   -- CharArr : used to access FITS-header
   -- all other xxxxArr : used to access FITS-data

   procedure Read (FitsStream : in Ada.Streams.Stream_IO.Stream_Access;
                   Data       : in out DataArray_Type);

   procedure Write (FitsStream : in Ada.Streams.Stream_IO.Stream_Access;
                    Data       : in DataArray_Type);

-- procedure Read_Header (Stream,Header_Type);
-- procedure Write_Header(Stream,Header_Type);

-- procedure Read_Data (Stream,Data_Array,Offset);
-- procedure Write_Data(Stream,Data_Array,Offset);
   -- Offset counted in Data_Type relative to start of Data_Unit (after the Header)

-- for Header_Type'Read  use Read_Header;
-- for Header_Type'Write use Write_Header;

-- for Data_Type'Read  use Read_Data;
-- for Data_Type'Write use Write_Data;


   ------------------------------------------
   -- List FITS FIle content : HDU params  --
   ------------------------------------------

   MaxAxes : constant Positive := 999; -- [FITS, Sect 4.4.1]
   subtype NAXIS_Type is Natural range 0 .. MaxAxes;
   -- [FITS 4.4.1.1 Primary Header] "A value of zero signifies
   -- that no data follow the header in the HDU."

   type Dim_Type is array (1..MaxAxes) of Positive;-- FITS poses no limit on max value of NAXISi
   type HDU_Info_Type is record
      CardsCnt : Positive;    -- number of cards in this Header
      Data     : FITSData_Type; -- data type as given by BITPIX
      BitPix   : Positive;    -- BITPIX from Header (data size in bits)
      Naxes    : NAXIS_Type;  -- NAXIS  from header
      Naxis    : Dim_Type;    -- NAXISi from header, 0 means dimension not in use
   end record;

   Null_HDU_Info : constant HDU_Info_Type := (1,Int32,4,0,(others=>0));

   type HDU_Info_Arr is array (Positive range <>) of HDU_Info_Type;

   procedure List_Content(FitsFile   : in Ada.Streams.Stream_IO.File_Type;
                          HDUInfoArr : in out HDU_Info_Arr) is null;
   -- list HDU properties (Cards, Data Type and dimensionality)

end FITSStream;

---------------
-- Notes:

-- function Index(Stream) return Positive_Count; <- maybe not needed??
   -- returns HDU_Num within which the Stream_IO.Index currently resides
