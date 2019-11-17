
-- hogh level block/HDU copying

package FITS.File.Misc is


   -----------
   -- Misc: --
   -----------

   function DU_Size (NAXISArr : in NAXIS_Arr)
     return FPositive;

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



end FITS.File.Misc;
