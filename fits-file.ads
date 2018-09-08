

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

