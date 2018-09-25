
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

   type Coord_Arr    is array (FPositive range <>) of FPositive;
   type Element_Type is (Char, UInt8, Int16, Int32, Int64, Float32, Float64);

   procedure Set_Index(FitsFile    : in SIO.File_Type;
                       HDUNum      : in Positive;
                       Coord       : in Coord_Arr := (1,1);
                       ElementType : in Element_Type := Char);

   -- FITS-file content info

   type HDU_Info_Type(NAXIS : Positive) is record
      XTENSION : String(1..10);   -- XTENSION string or empty
      CardsCnt : FPositive;       -- number of cards in this Header
      BITPIX   : Integer;              -- data type
      NAXISn   : NAXIS_Type(1..NAXIS); -- data dimensions
   end record;

   function  Get (FitsFile : in  SIO.File_Type)
     return HDU_Info_Type;

   -- Read Header Cards

   procedure Read_Card  (FitsFile  : in  SIO.File_Type;
                         Card      : out Card_Type ) is null;

   procedure Read_Cards (FitsFile  : in  SIO.File_Type;
                         Cards     : out Card_Block ) is null;

   -- Read Data Unit

   type UInt8_Arr   is array ( FPositive range <> ) of Unsigned_8;
   type Int16_Arr   is array ( FPositive range <> ) of Integer_16;
   type Int32_Arr   is array ( FPositive range <> ) of Integer_32;
   type Int64_Arr   is array ( FPositive range <> ) of Integer_64;
   type Float32_Arr is array ( FPositive range <> ) of Float_32;
   type Float64_Arr is array ( FPositive range <> ) of Float_64;
   -- FITS.Data has BigEndian conversion for Float32 & 64

   function Element(Data  : in UInt8_Arr;
                    Coord : in Coord_Arr) return Unsigned_8;

   function Element(Data  : in Float32_Arr;
                    Coord : in Coord_Arr) return Float_32;


   procedure Read_Data (FitsFile : in  SIO.File_Type;
                        Size     : in  Coord_Arr;
                        Data     : out UInt8_Arr) is null;

   -- ... for all types ...

   procedure Read_Data (FitsFile : in  SIO.File_Type;
                        Size     : in  Coord_Arr;
                        Data     : out Float32_Arr) is null;

   -- Write

   package Max_8 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max =>  8);
   package Max20 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 20);
   package Max48 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 48);
   package Max70 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 70);

   function To_Card (Key     : in Max_8.Bounded_String;
                     Value   : in Max20.Bounded_String;
                     Comment : in Max48.Bounded_String)
                     return Card_Type;
    -- for cards with value

   function To_Card (Key     : in Max_8.Bounded_String;
                     Comment : in Max70.Bounded_String)
                     return Card_Type;
    -- for cards with text (like HISTORY or COMMENT, see FITS 4.1.2.2)

    -- FIXME add one more: for cards with Key and long Value (see FITS 4.1.2.3)

   procedure Write_Card  (FitsFile : in SIO.File_Type;
                          Card     : in Card_Type) is null;

   procedure Write_Cards (FitsFile : in SIO.File_Type;
                          Cards    : in Card_Arr) is null;

   -- FIXME which is preferred from the 2 below:
   -- (Write_ENDCard will also do padding)
   procedure Write_ENDCard(FitsFile : in SIO.File_Type);
   procedure Write_Padding(FitsFile : in SIO.File_Type);
   -- must be called right after Write_Cards when END Card was written
   -- File Index must not change between the two calls

   generic
    type Item is private;
    type Item_Arr is array (FPositive range <>) of Item;
    with function Element (Coord : in Coord_Arr) return Item;
   procedure Write_Data (FitsFile  : in  SIO.File_Type;
                         MaxCoords : in  Coord_Arr);

   -- Notes:
   -- Write_Data writes the complete DataUnit
   -- ?? Write_Data first moves to begining of next Block (writes Header padding)
   -- Write_Data adds DataUnit padding after last Item written

-- END new IF


   BlockSize_bits : constant FPositive := 2880 * Byte'Size; -- 23040 bits
   -- [FITS 3.1 Overall file structure]


   function  DU_Size_blocks  (InFits  : in SIO.File_Type) return FNatural;
    -- calls Parse_HeaderBlocks & FITS.Size.Size_blocks

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

