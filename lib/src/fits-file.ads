
-- Interface func description:
--
-- this library supports RANDOM access in reading and SEQUENTIAL writing
-- of FITS files
--
-- Note:
-- high level user IF for Read has 2 variants:
-- 1 can read/write one object of Header_Type -> such
--   func (Header'Read/'Write attrib) should handle padding
-- 2 if Header considered "big" -> user has to read/write it in cycle by
--   CardBlocks (Card_Arr'Write/'Read which is defined by language).
--   CardBlock has 32 cards, so it covers the padding.
--
-- Set_Index(in Stream, in HDUNum)
-- position file pointer to begining of Header of the given HDU
--
-- HDU info struct: xtension name, cards cnt, data type, dimensions per axis
-- Get(in Stream, in HDUNum, out HDU_Info_Record)
--
--
-- Read (user knows which coords' data he wants - but does not know
--       where in file it is - and he has not the data)
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
--       furure yes: introduce class NCube which will unite Coord and DataArr[]
--
-- 6x function Element(DataArr[<>],(I1,I2,...)) returns Element_Type
--
--
-- Write (user has the data and knows its coordinates
--        but does not know where to write it)
--
-- Writing is alway SEQUENTIAL. So below function need to be called in a loop
-- until all data is written.
--
-- Write_Cards(in Stream, in  CardArr[N])
-- Write_Padding(in Stream)

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

-- Extensibility:
-- new groups of keywords in Header (like WCS coords related is now standardized)
-- new Extension HDU types (now: IMAGE, TABLEBIN/ASCII)
-- new Data Unit data-type? (now: UInt8, Int16/32/63, Float32/64)

-- Note: using Read/Write_Card instead of 'Read 'Write attribs
-- does not forcs use of Stream_IO in client sw (one could imoplement
-- Read_Card also on Direct_IO Text_IO or or other)
-- However we have SIO.File_Type we should do FITS.File_Type and
-- package FITS renames Ada.Streams.Stream_IO; this allows with
-- one line change to do:
-- package FITS renames Ada.Dierect_IO; and new body with Direct_IO
-- implementation of Read/Write_Card and others...
-- OR even better (rename only File_Type not all Package):
-- type File_Type is new SIO.File_Type;
-- then we'd have: FITS.File.File_Type

-- Note on Read_Data() API usage:
-- This access enables data modification of each data element separately.
-- If modified data element depends on more (maybe neighbpuring) data,
-- this simple interface is cumbersome. Better to use NCube, where access
-- is governed by coordinates.


with Ada.Streams.Stream_IO;

package FITS.File is

   package SIO renames Ada.Streams.Stream_IO;

   procedure Set_Index(FitsFile : in SIO.File_Type;
                       HDUNum   : in Positive);


   -----------------------
   -- FITS-file content --
   -----------------------

   type HDU_Info_Type(NAXIS : Positive) is record
      XTENSION : String(1..10);   -- XTENSION string or empty
      CardsCnt : FPositive;       -- number of cards in this Header
      BITPIX   : Integer;             -- data type
      NAXISn   : NAXIS_Arr(1..NAXIS); -- data dimensions
   end record;

   function  Get (FitsFile : in  SIO.File_Type)
      return HDU_Info_Type;

   function DU_Size (NAXISArr : in NAXIS_Arr)
     return FPositive;


   --------------------------------
   -- Read Header Cards and Data --
   --------------------------------

   -- Read cards one card at a time or by blocks of 36 cards.
   -- ** FITS standard guarantees that a healthy FITS file has at least
   -- ** 36 cards from start.

   function Read_Card  (FitsFile  : in  SIO.File_Type)
     return Card_Type;

   function Read_Cards (FitsFile  : in  SIO.File_Type)
     return Card_Block;


   -- Read data by free-sized data-chunks into 1-dim array
   -- max size is known from header Get(HDU_Info): NAXIS1 x NAXIS2 x ...
   -- ** FITS standard guarantees that healthy FITS File has that many data.

   generic
     type Data_Arr(<>) is private;
   procedure gen_Read_Data (FitsFile : in  SIO.File_Type;
                            Data     : in out Data_Arr);
   -- Data_Arr is any of FITS.UInt8_Arr ... FITS.Float64_Arr


   -----------------------------------------------
   -- Write Header Cards and Data (and Padding) --
   -----------------------------------------------

   procedure Write_Card  (FitsFile : in SIO.File_Type;
                          Card     : in Card_Type);

   procedure Write_Cards (FitsFile : in SIO.File_Type;
                          Cards    : in Card_Arr);

   generic
     type Data_Arr(<>) is private;
   procedure gen_Write_Data (FitsFile : in SIO.File_Type;
                             Data     : in Data_Arr);
   -- Data_Arr is any of FITS.UInt8_Arr ... FITS.Float64_Arr

   generic
    type Item is private;
    type Item_Arr is array (FPositive range <>) of Item;
    with function Element (Coord : in NAXIS_Arr) return Item;
   procedure Write_DataUnit (FitsFile  : in  SIO.File_Type;
                             MaxCoords : in  NAXIS_Arr);
   -- Notes:
   -- Write_Data writes the complete DataUnit
   -- Write_Data adds DataUnit padding after last Item written

   -- FIXME why not to offer also Write_Data(S, Data_Arr) API - analogues
   --       to Read_Data(S,DataChunk). 6x for all data arrary types.


   HeaderPadValue : constant Unsigned_8 := 32; -- Space ASCII value
   DataPadValue   : constant Unsigned_8 :=  0;

   procedure Write_Padding(FitsFile : in SIO.File_Type;
                           From     : in SIO.Positive_Count;
                           PadValue : in Unsigned_8);
   -- [FITS ??]: FITS file consists of 2880-bytes long blocks.
   -- If last Header- or Data-block is not filled up,
   -- Write_Padding puts PadValue from FileOffset until end of the block.
   -- If Block is filled up, Write_Padding does nothing.


   -- Misc:

   --
   -- calc DataUnit size in blocks
   --
   function  DU_Size_blocks  (InFits  : in SIO.File_Type) return FNatural;
   -- FIXME is this needed ?? see commands.adb: Couldn't be used Get() & Size_blocks() ??

   --
   -- copy NBlocks from current index position in chunks of ChunkSize_blocks
   --
   procedure Copy_Blocks (InFits  : in SIO.File_Type;
                          OutFits : in SIO.File_Type;
                          NBlocks : in FPositive;
                          ChunkSize_blocks : in Positive := 10);
   -- FIXME is this needed ? maybe should be internal only
   --       Ext API: Copy_HDU ok.

   --
   -- copy HDU from InFile to OutFile: both file-pointers must be correctly positioned
   --
   procedure Copy_HDU (InFits  : in SIO.File_Type;
                       OutFits : in SIO.File_Type;
                       HDUNum  : in Positive;
                       ChunkSize_blocks : in Positive := 10);
   -- FIXME IF: move to some new FITS.Utils or similar

end FITS.File;
