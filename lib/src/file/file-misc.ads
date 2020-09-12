
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

--with V3_Types; -- Unsigned_8 needed for Padding value

package File.Misc is

    BlockSize_bits : constant Positive_Count := 23040;--2880 * Byte'Size; -- 23040 bits
    -- [FITS 3.1 Overall file structure]

   -------------------------------
   -- Write Header/Data Padding --
   -------------------------------

--   HeaderPadValue : constant V3_Types.Unsigned_8 := 32; -- Space ASCII value
--   DataPadValue   : constant V3_Types.Unsigned_8 :=  0;

--   procedure Write_Padding(FitsFile : in SIO.File_Type;
--                           From     : in SIO.Positive_Count;
--                           PadValue : in V3_Types.Unsigned_8);
   -- [FITS ??]: FITS file consists of 2880-bytes long blocks.
   -- If last Header- or Data-block is not filled up,
   -- Write_Padding puts PadValue from FileOffset until end of the block.
   -- If Block is filled up, Write_Padding does nothing.


  function  DU_Size_blocks (FitsFile : in SIO.File_Type) return Count; 
        -- used in commands




   ---------------------------
   -- Ops on two FITS-files --
   ---------------------------

   -- high level block/HDU copying

   --  
   -- copy NBlocks from current index position in chunks of ChunkSize_blocks
   --  
   procedure Copy_Blocks (InFits  : in SIO.File_Type;
                          OutFits : in SIO.File_Type;
                          NBlocks : in Positive_Count;
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



end File.Misc;
