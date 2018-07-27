

with Ada.Streams.Stream_IO;

with FITS.Size; use FITS.Size;
with FITS.Header; use FITS.Header;

package FITS.File is

   package SIO renames Ada.Streams.Stream_IO;

   BlockSize_bits : constant FPositive := 2880 * Byte'Size; -- 23040 bits
   -- [FITS 3.1 Overall file structure]

   --
   -- Read File until ENDCard found,
   -- cal Parse_Card for each card and
   -- return count of Cards
   --
   generic
     type Parsed_Type is limited private;
     with procedure Parse_Card
                    (Card : in Card_Type;
                     Data : out Parsed_Type);
   procedure  Read_Header_Blocks
             (FitsFile : in SIO.File_Type;
              Data     : out Parsed_Type;
              CardsCnt : out FNatural);


   procedure Parse_HeaderBlocks (FitsFile : in SIO.File_Type;
                                 HDUSize  : out HDU_Size_Type);
    -- extract HDU-size information: read by Blocks.
    -- After this call file-pointer points to DU (or next HDU)




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

