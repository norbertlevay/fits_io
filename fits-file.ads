
-- Interface func description:
-- this library supports SEQUENTIAL reading/writing of FITS files
-- (no random access possible)
-- Note: Actually we could allow random access for reading, assuming file does not change.

-- Set_Index(in Stream, in HDUNum)
-- position file pointer to begining of Header of the given HDU

-- HDU info struct: xtension name, cards cnt, data type, dimensions per axis
-- Get(in Stream, in HDUNum, out HDU_Info_Record)

-- HDU-Data access is ALWAYS SEQUENTIAL
-- (repeated reads/writes - no positioning - read/write consecutive data):
-- user has to use below funcs in cycle until
-- Header-end reached (known by END-card),
-- DU-end reached (size known from Header when Reading).
--
-- User may position to given HDU (Set_Index),
-- but cannot position within HDU.

-- Read_Cards (in Stream, in N, out CardArr) <- when ENDCard read, skip padding to position to beginig of DU
-- Write_Cards(in Stream, in CardArr) <- if CardArr contains ENDCard do padding

-- Below funcs have 6 variants of DataArr for all
-- DU data types (UInt8 Int16/32/64, Float32/64):
--
-- 6x Read_Data (in Stream, in N, out DataArr) <- padding no issue: always use Set_index to position to Header
-- 6x Write_Data(in Stream, in DataArr, in Bool Last=False) <- If DataArr reached DU full size (Last=True), do pading
-- or
-- 6x Write_Data(in Stream, in DataArr) <- Use Write_Padding (same as above with Last=False)
-- Write_Padding(in Stream) <- to be called after last Write_Data (with all having Last=False)
--  (uses file index to pad until next multiple of BlockSize)

-- NEXT:
-- DataArr means 1-dimensional represenation of N-dimensional data.
-- How to do similar interface funcs with NCube ? like in PNG:
-- -> user gives 'func Data(indeces) returns DataType' which we call
-- when reading/writing


with Ada.Streams.Stream_IO;

with FITS.Size;  use FITS.Size;
--with FITS.Header; use FITS.Header;

package FITS.File is

   package SIO renames Ada.Streams.Stream_IO;

   BlockSize_bits : constant FPositive := 2880 * Byte'Size; -- 23040 bits
   -- [FITS 3.1 Overall file structure]

   --
   -- returns information about one HDU
   --
   type DimArr_Type is array (1 .. 999) of FPositive; -- size of data cube (aka NAXIS NAXISn)
   -- FIXME allow memory optimization by specifying 1..MaxAxes < 999
   -- Print as implementation limit
   type HDU_Info_Type is record
      XTENSION   : String(1..10); -- XTENSION type string or empty
      CardsCnt   : FPositive;     -- number of cards in this Header (gives Header-size)

      BITPIX     : Integer;     -- DataType (aka BITPIX)
      Dimensions : DimArr_Type;
   end record;
   -- FIXME replace HDU_Size_Type with HDU_Info_Type in this ads:
   -- (allows to remove with FITS.Size from the interface)

   procedure Parse_HeaderBlocks (FitsFile : in SIO.File_Type;
                                 HDUSize  : out HDU_Size_Type);




   function  DU_Size_blocks  (InFits  : in SIO.File_Type) return FNatural;
    -- calls Parse_HeaderBlocks & FITS.Size.Size_blocks

   procedure List_Content (FitsFile : in SIO.File_Type;
                           Print : not null access
                           procedure(HDUNum : Positive;
                                     HDUSize : HDU_Size_Type) );
   -- list each HDU's size related parameters

   procedure Set_Index (FitsFile : in SIO.File_Type;
                        HDUNum   : in Positive);
   -- set file-index to correct position before 'Read/'Write

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

