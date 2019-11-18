
-- hogh level block/HDU copying

package FITS.File.Misc is

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


end FITS.File.Misc;
