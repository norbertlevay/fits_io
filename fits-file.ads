
-- Interface func description:
--
-- this library supports RANDOM access in reading and SEQUENTIAL writing
-- of FITS files
--
-- Set_Index(in Stream, in HDUNum)
-- position file pointer to begining of Header of the given HDU
--
-- HDU info struct: xtension name, cards cnt, data type, dimensions per axis
-- Get(in Stream, in HDUNum, out HDU_Info_Record)
--
--
-- Read (user knows which coords' data he wants, but has not the data)
--
-- Reading is RANDOM ACCESS, user may position to given HDU (Set_Index),
-- and then specify an OFFSET (Set_Offset). Offset is counted from the
-- begining of the Header or DataUnit.
--
-- Set_Offset(Stream, CardIndex)          <- move in Stream to Card given by CardIndex
-- Set_Offset(Stream, CharIndexVector[2]) <- move in Stream to Character given by 2-dimensional CharIndexVector
-- Set_Offset(Stream, ElementIndexVector[N], ElementType)<- move in Stream to Element given by ElementIndex
--
-- Read_Card (in Stream, out Card)
-- Read_Card (in Stream, out CardArr[M])
-- -- reads only upto ENDCard
-- -- if ENDCard read, Read_Card() will skip the padding and move
-- -- file index to begining of the Data Unit
--
-- 6x Read_Data (in Stream, in (N1,N2,...), out DataArr[<>])
-- -- DataArr is 1-dimensional and has 6 variants: UInt8 Int16/32/64, Float32/64 and is of size N1*N2*...
-- -- (N1,N2,...) is vector which specifies the size of the
-- --  data cube to be read
-- -- It is responsibility of the caller to ensure that we do not
-- -- read behind data unit end (Set_Offset + (N1*N2*...) < DUEnd).
--
-- FIXME we should hide DataArr[] implementation ??
--
-- 6x function Element(DataArr[<>],(I1,I2,...)) returns Element_Type
--
--
-- Write (user has the data but does not know where to write it)
--
-- Writing is alway SEQUENTIAL. So below function need to be called in a loop
-- until all data is written.
--
-- Write_Cards(in Stream, in  CardArr[N])
   -- if CardArr[N] contains ENDCard do padding

-- generic
--  ElementType : UInt8 INT16/32/64 Float32/64
--  function Element(Coord : Vector[Dims]) return ElementType;
-- Write_Data(in Stream);
--
-- -- user has to supply the implementation of the Element() function
-- -- Element() function return one data element in coordinats given
-- -- in vector Vector[Dims]
--
-- Misc
--
-- function Card(key: in String,value: in String,comment=null: in String) return Card_Type
--

with Ada.Streams.Stream_IO;
with Ada.Strings.Bounded;

with FITS.Data;  use FITS.Data;-- DU Types
with FITS.Size;  use FITS.Size;
--with FITS.Header; use FITS.Header;

   -- FIXME replace HDU_Size_Type with HDU_Info_Type in this ads:
   -- (allows to remove with FITS.Size from the interface)

package FITS.File is

-- BEGIN new IF
   package SIO renames Ada.Streams.Stream_IO;

   -- FITS-file content info

   NAXIS_Max : constant Positive := 999;

   type NAXISn_Type is array (1 .. NAXIS_Max) of FPositive;

   type HDU_Info_Type is record
      XTENSION : String(1..10);   -- XTENSION string or empty
      CardsCnt : FPositive;       -- number of cards in this Header
      BITPIX   : Integer;     -- data type
      NAXISn   : NAXISn_Type; -- data length
   end record;

   procedure Get (FitsFile : in  SIO.File_Type;
                  HDUNum   : in  Positive;
                  HDUInfo  : out HDU_Info_Type) is null;

   -- positioning

   procedure Set_Index_newif(FitsFile  : in SIO.File_Type;
                             HDUNum    : in Positive;
                             CardIndex : in Positive := 1) is null;

   type Coord_Arr    is array (Positive range <>) of Positive;
   type Element_Type is (UInt8, Int16, Int32, Int64, Float32, Float64);

   procedure Set_Index(FitsFile    : in SIO.File_Type;
                       HDUNum      : in Positive;
                       Coord       : in Coord_Arr;
                       ElementType : in Element_Type) is null;

   -- Header Cards

   type Card_Type_newif is array (1 .. 80) of Character;
   type Card_Arr_newif  is array (Positive range <>) of Card_Type_newif;

   ENDCard_newif : constant Card_Type_newif := ( 1=>'E', 2=>'N', 3=>'D', others => ' ');

   package Max_8                          -- FIXME check lengths:
     is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 8);
   package Max11
     is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 11);
   package Max59
     is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 80-21);

   function To_Card (Key     : in Max_8.Bounded_String;
                     Value   : in Max11.Bounded_String;
                     Comment : in Max59.Bounded_String)
                     return Card_Type_newif;

   -- Data Unit

   type UInt8_Arr   is array ( FPositive range <> ) of Unsigned_8;
   type Int16_Arr   is array ( FPositive range <> ) of Integer_16;
   type Int32_Arr   is array ( FPositive range <> ) of Integer_32;
   type Int64_Arr   is array ( FPositive range <> ) of Integer_64;
   type Float32_Arr is array ( FPositive range <> ) of Float_32;
   type Float64_Arr is array ( FPositive range <> ) of Float_64;
   -- FITS.Data has BigEndian conversion for FLoat32 & 64

   function Element(Data  : in UInt8_Arr;
                    Coord : in Coord_Arr) return Unsigned_8;

   function Element(Data  : in Float32_Arr;
                    Coord : in Coord_Arr) return Float_32;



   -- Read

   procedure Read_Cards  (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                          Cards  : out Card_Arr_newif) is null;
   for Card_Arr_newif'Read  use Read_Cards;

   procedure Read_Data (FitsFile : in  SIO.File_Type;
                        Size     : in  Coord_Arr;
                        Data     : out UInt8_Arr) is null;

   procedure Read_Data (FitsFile : in  SIO.File_Type;
                        Size     : in  Coord_Arr;
                        Data     : out Float32_Arr) is null;

   -- Write

   procedure Write_Cards (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                          Cards  : in  Card_Arr_newif) is null;
   for Card_Arr_newif'Write use Write_Cards;

   generic
    type Item is private;
    type Item_Arr is array (FPositive range <>) of Item;
    with function Element (Coord : in Coord_Arr) return Item;
   procedure Write_Data (FitsFile : in  SIO.File_Type);

   -- Notes:
   -- Write_Data writes the complete DataUnit
   -- Write_Data first moves to begining of next Block (writes Header padding)
   -- Write_Data adds DataUnit padding after last Item written

-- END new IF


   procedure Set_Index(FitsFile  : in SIO.File_Type;
   		       HDUNum    : in Positive);

   BlockSize_bits : constant FPositive := 2880 * Byte'Size; -- 23040 bits
   -- [FITS 3.1 Overall file structure]


   procedure Parse_HeaderBlocks (FitsFile : in SIO.File_Type;
                                 HDUSize  : out HDU_Size_Type);




   function  DU_Size_blocks  (InFits  : in SIO.File_Type) return FNatural;
    -- calls Parse_HeaderBlocks & FITS.Size.Size_blocks

   procedure List_Content (FitsFile : in SIO.File_Type;
                           Print : not null access
                           procedure(HDUNum : Positive;
                                     HDUSize : HDU_Size_Type) );
   -- list each HDU's size related parameters

   --
   -- copy NBlocks from current index position in chunks of ChunkSize_blocks
   --
   procedure Copy_Blocks (InFits  : in SIO.File_Type;
                          OutFits : in SIO.File_Type;
                          NBlocks : in FPositive;
                          ChunkSize_blocks : in Positive := 10);

   --
   -- copy HDU from InFile to OutFile: both file-pointers must be correctly positioned
   --
   procedure Copy_HDU (InFits  : in SIO.File_Type;
                       OutFits : in SIO.File_Type;
                       HDUNum  : in Positive;
                       ChunkSize_blocks : in Positive := 10);

end FITS.File;

